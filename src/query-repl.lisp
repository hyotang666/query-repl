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
            (lambda (x) (print x *query-io*) (force-output *query-io*))
          (query-eval (query-read)))))

(defmacro query-case (query &body clauses)
  `(pcs:restart-case (progn ,query (force-output *query-io*) (query-repl))
     ,@clauses))
