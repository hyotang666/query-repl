(in-package :cl-user)

(defpackage :query-repl
  (:use :cl)
  (:shadowing-import-from :portable-condition-system
                          #:invoke-restart-interactively
                          #:restart-name
                          #:restart-case)
  (:shadow compute-restarts)
  (:export ;;;; Main api
           #:query-case
           #:select
           #:paged-select
           #:*query-eval*))

(in-package :query-repl)

(define-condition query () ())

(defparameter *query-eval* t)

(defparameter *prompt* ">")

(declaim (type boolean *query-eval*))

(defun compute-restarts ()
  (remove-if (lambda (condition) (not (typep condition 'query)))
             (cl:compute-restarts (load-time-value (make-condition 'query) t))))

(defun query-eval (exp)
  (let ((restarts (compute-restarts)))
    (typecase exp
      (integer
       (let ((restart (nth exp restarts)))
         (when restart
           (invoke-restart-interactively restart))))
      (symbol
       (let ((restarts
              (loop :for restart :in restarts
                    :when (uiop:string-prefix-p exp (restart-name restart))
                      :collect restart)))
         (typecase restarts
           (null) ; do nothing.
           ((cons restart null) (invoke-restart-interactively (car restarts)))
           (otherwise
            (progn
             (warn "~S is ambiguous:~2I~:@_~{~A~^~:@_~}" exp
                   (mapcar #'restart-name restarts))
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
  (let ((restarts (compute-restarts)))
    (when restarts
      (loop :with max
                  := (reduce #'max restarts
                             :key (lambda (x)
                                    (length (string (restart-name x)))))
            :for i :upfrom 0
            :for restart :in restarts
            :do (format t "~%~3D: [~VA] ~A" i max (restart-name restart)
                        restart)))
    (format t "~%~A " *prompt*)
    (force-output t)))

(defun query-read (&optional (*standard-input* *query-io*))
  (if *query-eval*
      (read)
      (handler-case
          (let (*read-eval*)
            (read))
        ((or reader-error serious-condition) (condition)
          (warn "Ignore: ~A" condition)))))

(defun query-repl ()
  (loop (query-prompt)
        (multiple-value-call
            (lambda (&rest args)
              (dolist (arg args)
                (print arg *query-io*)
                (force-output *query-io*)))
          (query-eval (query-read)))))

(defmacro query-case (&whole whole query &body clauses)
  (check-bnf:check-bnf (:whole whole)
    ((query check-bnf:expression))
    (((clause+ clauses) (name check-bnf:<lambda-list> restart-option* body*))
     (name symbol)
     (restart-option* restart-option-key check-bnf:expression)
     (restart-option-key (member :test :interactive :report))
     (body check-bnf:expression)))
  `(restart-case (progn ,query (force-output *query-io*) (query-repl))
     ,@clauses))

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
  (labels ((tester (condition)
             (typep condition 'query))
           (reporter (elt)
             (lambda (s) (format s "~S" elt)))
           (returner (elt)
             (lambda () (return-from select elt)))
           (rec (list)
             (if (endp list)
                 (query-repl)
                 (restart-bind ((select (returner (car list))
                                        :test-function #'tester
                                        :report-function (reporter (car list))))
                   (rec (cdr list))))))
    (rec (reverse list))))

(defun paged-select (list &key (max 10) (key #'identity))
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
