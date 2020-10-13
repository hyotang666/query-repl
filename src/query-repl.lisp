(in-package :cl-user)

(defpackage :query-repl
  (:use :cl)
  (:export ;;;; Main api
           #:query-case))

(in-package :query-repl)

(define-condition query () ())

(defun query-eval (exp)
  (let ((restarts
         (pcs:compute-restarts (load-time-value (make-condition 'query) t))))
    (typecase exp
      (integer
       (let ((restart (nth exp restarts)))
         (when restart
           (pcs:invoke-restart-interactively restart))))
      (keyword
       (let ((restarts
              (loop :for restart :in restarts
                    :when (uiop:string-prefix-p exp (pcs:restart-name restart))
                      :collect restart)))
         (typecase restarts
           (null) ; do nothing.
           ((cons pcs:restart null)
            (pcs:invoke-restart-interactively (car restarts)))
           (otherwise
            (progn
             (warn "~@<~S is ambiguous:~2I~:@_~{~A~^~:@_~}~:>" exp
                   (mapcar #'pcs:restart-name restarts))
             (return-from query-eval (values)))))))))
  (let ((results
         (multiple-value-list
          (let ((- exp))
            (eval exp)))))
    (shiftf /// // / results)
    (shiftf *** ** * (car results))
    (shiftf +++ ++ + exp)
    (values-list results)))

(defun query-read (&optional (*standard-input* *query-io*))
  (let ((restarts
         (pcs:compute-restarts (load-time-value (make-condition 'query) t))))
    (when restarts
      (loop :with max
                  := (reduce #'max restarts
                             :key (lambda (x)
                                    (length (string (pcs:restart-name x)))))
            :for i :upfrom 0
            :for restart :in restarts
            :do (format *query-io* "~%~3D: [~VA] ~A" i max
                        (pcs:restart-name restart) restart)))
    (format *query-io* "~%> ")
    (force-output *query-io*)
    (read)))

(defun query-repl ()
  (loop (multiple-value-call
            (lambda (&rest args)
              (dolist (arg args)
                (print arg *query-io*)
                (force-output *query-io*)))
          (query-eval (query-read)))))

(defmacro query-case (query &body clauses)
  `(pcs:restart-case (progn ,query (force-output *query-io*) (query-repl))
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
