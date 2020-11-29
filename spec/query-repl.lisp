(defpackage :query-repl.spec
  (:use :cl :jingoh :query-repl))
(in-package :query-repl.spec)
(setup :query-repl)

(requirements-about *PROMPT* :doc-type variable)

;;;; Description:

;;;; Value type is T.
#? *PROMPT* :be-the T

; Initial value is `">"`

;;;; Affected By:
; QUERY-PROMPT

;;;; Notes:

