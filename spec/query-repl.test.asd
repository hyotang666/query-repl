; vim: ft=lisp et
(in-package :asdf)
(defsystem "query-repl.test"
  :version
  "0.11.3"
  :depends-on
  (:jingoh "query-repl")
  :components
  ((:file "query-repl"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :query-repl args)))
