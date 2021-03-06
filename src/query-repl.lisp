(in-package :cl-user)

(defpackage :query-repl
  (:use :cl)
  (:export ;;;; Main api
           #:query-case
           #:select
           #:paged-select
           #:*query-eval*)
  (:export ;;;; Underlying helpers.
           #:*prompt*
           #:query-prompt
           #:query-read
           #:query-eval
           #:query-repl
           #:query-bind))

(in-package :query-repl)

(defstruct selection
  (name nil :type symbol :read-only t)
  (report-function (error "Required") :type function :read-only t)
  (interactive-function (error "Required") :type function :read-only t))

(defvar *selections* nil)

(defparameter *query-eval* t)

(defparameter *prompt* ">")

(declaim (type boolean *query-eval*))

(defun query-eval (exp)
  (let ((selections *selections*))
    (typecase exp
      (integer
       (let ((selection (nth exp selections)))
         (when selection
           (funcall (selection-interactive-function selection)))))
      (symbol
       (let ((selections
              (loop :for selection :in selections
                    :when (uiop:string-prefix-p exp (selection-name selection))
                      :collect selection)))
         (typecase selections
           (null) ; do nothing.
           ((cons selection null)
            (funcall (selection-interactive-function (car selections))))
           (otherwise
            (progn
             (warn "~S is ambiguous:~2I~:@_~{~A~^~:@_~}" exp
                   (mapcar #'selection-name selections))
             (return-from query-eval (values)))))))))
  (let ((results
         (multiple-value-list
          (let ((- exp))
            (if *query-eval*
                (eval exp)
                exp)))))
    (shiftf /// // / results)
    (shiftf *** ** * (car results))
    (shiftf +++ ++ + exp)
    (values-list results)))

(defun query-prompt (&optional (*standard-output* *query-io*))
  (assert (typep *standard-output* 'stream))
  (let ((selections *selections*))
    (when selections
      (loop :with max
                  := (reduce #'max selections
                             :key (lambda (x)
                                    (length (string (selection-name x)))))
            :for i :upfrom 0
            :for selection :in selections
            :do (format t "~%~3D: [~VA] " i max (selection-name selection))
                (funcall (selection-report-function selection)
                         *standard-output*)))
    (format t "~%~A " *prompt*)
    (force-output t)))

(defun query-read (&optional (*standard-input* *query-io*))
  (if *query-eval*
      (read)
      (let (*read-eval*)
        (read))))

(defun query-repl ()
  (loop (query-prompt)
        (multiple-value-call
            (lambda (&rest args)
              (dolist (arg args)
                (print arg *query-io*)
                (force-output *query-io*)))
          (handler-case (query-read)
            (condition (condition)
              (if *query-eval*
                  (error condition)
                  (progn (warn (princ-to-string condition)) (values))))
            (:no-error (read)
              (query-eval read))))))

(defmacro query-bind (&whole whole binds &body body)
  (check-bnf:check-bnf (:whole whole)
    (((bind* binds) (name body-function param*))
     (name symbol)
     (body-function check-bnf:expression)
     (param* query-param-key check-bnf:expression)
     (query-param-key (member :report-function :interactive-function)))
    ((body check-bnf:expression)))
  (flet ((<make-selection-form> (bind)
           (destructuring-bind
               (name function . rest)
               bind
             `(let ((function (coerce ,function 'function))
                    (reporter
                     (coerce
                       ,(or (getf rest :report-function)
                            `(lambda (s) (format s "~A" ',name)))
                       'function))
                    (reader
                     (coerce
                       ,(if (getf rest :interactive-function)
                            `(lambda ()
                               (apply ,function
                                      (funcall
                                        ,(getf rest :interactive-function))))
                            function)
                       'function)))
                ;; CLISP needs runtime check.
                (check-type function function)
                (check-type reporter function)
                (check-type reader function)
                (make-selection :name ',name
                                :report-function reporter
                                :interactive-function reader)))))
    `(let ((*selections*
            (list* ,@(mapcar #'<make-selection-form> binds) *selections*)))
       ,@body)))

(defmacro query-case (&whole whole query &body clauses)
  (check-bnf:check-bnf (:whole whole)
    ((query check-bnf:expression))
    (((clause+ clauses) (name check-bnf:<lambda-list> restart-option* body*))
     (name symbol)
     (restart-option* restart-option-key check-bnf:expression)
     (restart-option-key (member :interactive :report))
     (body check-bnf:expression)))
  (let ((block (gensym "QUERY")))
    `(block ,block
       (query-bind ,(mapcar
                      (lambda (clause) (<make-selection-form> clause block))
                      clauses)
         ,query
         (force-output *query-io*)
         (query-repl)))))

(defun <make-selection-form> (clause block)
  (let (reporter reader (name (car clause)) (lambda-list (second clause)))
    (multiple-value-bind (body decls)
        (alexandria:parse-body (cddr clause))
      (labels ((rec (list)
                 (if (endp list)
                     (finally list)
                     (body (car list) (cdr list))))
               (body (first rest)
                 (case first
                   ((:report)
                    (if rest
                        (progn (setf reporter (car rest)) (rec (cdr rest)))
                        (finally (cons first rest))))
                   ((:interactive)
                    (if rest
                        (progn (setf reader `#',(car rest)) (rec (cdr rest)))
                        (finally (cons first rest))))
                   (otherwise (finally (cons first rest)))))
               (finally (list)
                 `(,name
                   (lambda ,lambda-list
                     ,@decls
                     (return-from ,block (progn ,@list)))
                   :report-function
                   ,(typecase reporter
                      (string `(lambda (s) (format s ,reporter)))
                      (null `(lambda (s) (format s "~A" ',name)))
                      (otherwise `#',reporter))
                   :interactive-function ,reader)))
        (rec body)))))

(defun pprint-query-case (stream exp)
  (funcall
    (formatter
     #.(concatenate 'string "~:<" ; pprint-logical-block.
                    "~W~^ ~1I~@_" ; operator.
                    "~W~^ ~_" ; form.
                    "~@{" ; clauses.
                    "~/query-repl:pprint-query-case-clause/~^ ~_" ; each-clause
                    "~}" "~:>"))
    stream exp))

(defun pprint-query-case-clause (stream exp &rest noise)
  (declare (ignore noise))
  (if (atom exp)
      (write exp :stream stream)
      (pprint-logical-block (stream exp :prefix "(" :suffix ")")
        (pprint-exit-if-list-exhausted)
        (apply
          (formatter
           #.(concatenate 'string "~{~W~^ ~@_~:<~^~W~:>~}" ; pre.
                          "~@[" ; if exists.
                          " ~3I~_~{~W~^ ~@_~W~^ ~_~}" ; keys
                          "~]" "~^ ~1I" ; if exists body.
                          "~:*~:[~_~;~:@_~]" ; mandatory newline when keys.
                          "~@{~W~^ ~:@_~}")) ; body.
          stream (parse-query-clause exp)))))

(defun parse-query-clause (clause)
  (let ((pre
         (cons (car clause)
               (when (cdr clause)
                 (list (cadr clause))))))
    (loop :for list :on (cddr clause) :by #'cddr
          :while (and (keywordp (car list)) (cdr list))
          :collect (car list) :into keys
          :collect (cadr list) :into keys
          :finally (return (list* pre keys list)))))

(set-pprint-dispatch '(cons (member query-case)) 'pprint-query-case)

(defun select (list)
  (labels ((rec (list)
             (if (endp list)
                 (query-repl)
                 (query-bind ((select
                                (lambda () (return-from select (car list)))
                                :report-function (lambda (s)
                                                   (format s "~S" (car list)))))
                   (rec (cdr list))))))
    (rec (reverse list))))

(declaim
 (ftype (function (list &key (:max (integer 3 *)) (:key (or symbol function))))
        paged-select))

(defun paged-select (list &key (max 10) (key #'identity))
  ;; CLISP needs runtime check.
  (assert (typep list 'list))
  (assert (typep max '(integer 3 *)))
  (unless list
    (return-from paged-select list))
  (do ((vector (coerce list 'vector))
       (hash-table (make-hash-table))
       (tag-next '#:next)
       (tag-prev '#:prev)
       (length (length list))
       (index 0))
      (nil)
    (unless index
      (error "Internal error: Index is NIL."))
    (labels ((query (start end cont)
               (loop :for i :upfrom start :below end
                     :for (value exist?)
                          := (multiple-value-list (gethash i hash-table))
                     :if exist?
                       :collect value :into contents
                     :else
                       :collect (setf (gethash i hash-table)
                                        (funcall key (aref vector i)))
                         :into contents
                     :finally (funcall cont contents)))
             (prev-index (index)
               (let ((new (- index (- max 2))))
                 (if (= 1 new)
                     0
                     new)))
             (finaler (&key next tags)
               (lambda (contents)
                 (let ((selected (select (nconc tags contents))))
                   (cond
                     ((eq tag-prev selected) (setf index (prev-index index)))
                     ((eq tag-next selected) (setf index next))
                     (t (return selected)))))))
      (if (zerop index)
          (if (<= length max)
              (query index length (finaler))
              (query index (1- max)
                     (finaler :next (1- max) :tags (list tag-next))))
          (if (<= index length)
              (if (<= (- length index) (1- max))
                  (query index length (finaler :tags (list tag-prev)))
                  (query index (+ index (- max 2))
                         (finaler :next (+ index (- max 2))
                                  :tags (list tag-next tag-prev))))
              (error "Internal error. Index over the length."))))))
