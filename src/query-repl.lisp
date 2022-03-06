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

(declaim (optimize speed))

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
            :for i :of-type (integer 0 #.most-positive-fixnum) :upfrom 0
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
        (restart-case (multiple-value-call
                          (lambda (&rest args)
                            (dolist (arg args)
                              (print arg *query-io*)
                              (force-output *query-io*)))
                        (handler-case (query-read)
                          (condition (condition)
                            (if *query-eval*
                                (error condition)
                                (progn
                                 (warn (princ-to-string condition))
                                 (values))))
                          (:no-error (read)
                            (query-eval read))))
          (continue () :report "Return to query repl."))))

(defmacro query-bind (&whole whole binds &body body)
  (check-bnf:check-bnf (:whole whole)
    (((bind* binds) (name body-function param*))
     (name symbol)
     (body-function check-bnf:expression)
     (param* query-param-key check-bnf:expression)
     (query-param-key (member :report-function :interactive-function)))
    ((body check-bnf:expression)))
  #-check-bnf
  (mapc
    (lambda (bind)
      (check-type (car bind) symbol)
      (loop :for (k) :on (cddr bind) :by #'cddr
            :do (check-type k
                            (member :report-function :interactive-function))))
    binds)
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
  #-check-bnf
  (mapc
    (lambda (clause)
      (check-type (car clause) symbol)
      (check-type (second clause) list))
    clauses)
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
        (uiop:parse-body (cddr clause))
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
                                (lambda ()
                                  (declare (optimize (speed 1)))
                                  (return-from select (car list)))
                                :report-function (lambda (s)
                                                   (prin1 (car list) s))))
                   (rec (cdr list))))))
    (rec (reverse list))))

(declaim
 (ftype (function
         (boolean (integer 0 #xFFFF) (integer 0 #xFFFF)
          (mod #.most-positive-fixnum) simple-string)
         (values simple-string &optional))
        paged-prompt))

(defun paged-prompt (print-pagep max-page index length prompt)
  #+sbcl ; due to ratio.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if print-pagep
      (format nil "[~D/~D]~A" (ceiling (* max-page (/ (1+ index) length)))
              max-page prompt)
      prompt))

(declaim
 (ftype (function
         (list &key (:max (integer 3 #xFFFF)) (:key (or symbol function))
               (:print-page boolean)))
        paged-select))

(defun paged-select
       (list
        &key (max 10) (key #'identity) print-page
        &aux (key (coerce key 'function)))
  ;; CLISP needs runtime check.
  (assert (typep list 'list))
  (assert (typep max '(integer 3 *)))
  (unless list
    (return-from paged-select list))
  (prog* ((vector (coerce list 'vector)) (memo (make-hash-table))
          (tag-next '#:next) (tag-prev '#:prev) (length (length list))
          (max-page (ceiling length (- max 2))) (index 0) (prompt *prompt*))
    (declare ((integer 0 #xFFFF) index))
    (labels ((contents (start end)
               (loop :for i :of-type fixnum :upfrom start :below end
                     :for (value exist?)
                          := (multiple-value-list (gethash i memo))
                     :if exist?
                       :collect value
                     :else
                       :collect (setf (gethash i memo)
                                        (funcall key (aref vector i)))))
             (prev-index (index)
               (let ((new (- index (- max 2))))
                 (if (= 1 new)
                     0
                     new)))
             (selector (&key next tags)
               (lambda (contents)
                 (declare (optimize (safety 0)))
                 (let ((selected (select (nconc tags contents))))
                   (cond
                     ((eq tag-prev selected) (setf index (prev-index index)))
                     ((eq tag-next selected) (setf index next))
                     (t (return selected)))))))
      (declare
        (ftype (function ((integer 0 #xFFFF))
                (values (integer 0 #xFFFF) &optional))
               prev-index))
      (loop :for *prompt*
                 = (paged-prompt print-page max-page index length prompt)
            :do (if (zerop index) ; at the first page.
                    (if (<= length max) ; only have one page.
                        (funcall (selector) (contents index length))
                        ;; having some pages. We needs the next tag.
                        (funcall
                          (selector :next (1- max) :tags (list tag-next))
                          (contents index (1- max))))
                    (if (<= index length) ; not overflow.
                        (if (<= (- length index) (1- max)) ; at last page?
                            ;; We needs the prev tag.
                            (funcall (selector :tags (list tag-prev))
                                     (contents index length))
                            ;; Not in last page. We needs both prev and next tags.
                            (funcall
                              (selector :next (+ index (- max 2))
                                        :tags (list tag-next tag-prev))
                              (contents index (+ index (- max 2)))))
                        (error "Internal error. Index over the length.")))))))