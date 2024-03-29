; vim: ft=lisp et
(in-package :asdf)
(defsystem "query-repl"
  :version
  "4.1.4"
  :depends-on
  (
   "check-bnf" ; BNF like syntax checker.
   "uiop" ; Utilities.
   )
  :pathname
  "src/"
  :components
  ((:file "query-repl"))
  :author "SATO Shinichi"
  :license "MIT"
  :description "REPL for user query."
  :source-control (:git "git@github.com:hyotang666/query-repl")
  :bug-tracker "https://github.com/hyotang666/query-repl/issues")

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "query-repl").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "query-repl"))))
  (append (call-next-method) '((test-op "query-repl.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "query-repl")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "query-repl"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
