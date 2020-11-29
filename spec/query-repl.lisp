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

(requirements-about QUERY-PROMPT :doc-type function)

;;;; Description:

#+syntax (QUERY-PROMPT &optional (*standard-output* *query-io*)) ; => result

;;;; Arguments and Values:

; *standard-output* := Output stream, otherwise error.
#?(query-prompt "not-stream") :signals type-error

; result := NULL
#?(query-prompt) => NIL
,:stream nil

;;;; Affected By:
; *PROMPT*
#?(let ((*prompt* "input>"))
    (query-prompt))
:outputs "
input> "
,:stream *query-io*

;;;; Side-Effects:
; Outputs to `STREAM`.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *QUERY-EVAL* :doc-type variable)

;;;; Description:

;;;; Value type is BOOLEAN
#? *QUERY-EVAL* :be-the boolean

; Initial value is `T`

;;;; Affected By:
; QUERY-READ, QUERY-EVAL

;;;; Notes:

