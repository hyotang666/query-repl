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

(requirements-about QUERY-READ :doc-type function)

;;;; Description:
; Thin wrapper for CL:READ.

#+syntax (QUERY-READ &optional (*standard-input* *query-io*)) ; => result

;;;; Arguments and Values:

; *standard-input* := input stream, otherwise error.
#?(query-read "not stream") :signals type-error

; result := Lisp form.

;;;; Affected By:
; *QUERY-EVAL* *READ-EVAL* *READ-BASE* *READ-DEFAULT-FLOAT-FORMAT* *READ-SUPPRESS*

; When *QUERY-EVAL* is NIL, *READ-EVAL* is set NIL automatically.
#?(let ((*query-eval*))
    (with-input-from-string (*query-io* "#.(+)")
      (query-read)))
:signals reader-error

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about QUERY-EVAL :doc-type function)

;;;; Description:

#+syntax (QUERY-EVAL exp) ; => result

;;;; Arguments and Values:

; exp := T

; result := T

;;;; Affected By:
; *QUERY-EVAL*
; When *QUERY-EVAL* is NIL, EXP itself is returned.
#?(query-eval '(+)) => 0
#?(let ((*query-eval*))
    (query-eval '(+)))
=> (+)
,:test equal

; QUERY-REPL::*SELECTIONS* internal use.
; When EXP is integer and can get (nth EXP QUERY-REPL::*SELECTIONS*)
; such selections interactive-function is called.
#?(query-bind ((test (lambda () (princ :will-printed))))
    (query-eval 0))
:outputs "WILL-PRINTED"

; When EXP is symbol and can get only one slection from QUERY-REPL::*SELECTION* by its name
; such selections interactive-function is called.
#?(query-bind ((test (lambda () (princ :will-printed))))
    (query-eval :test))
:outputs "WILL-PRINTED"

; When EXP is symbol and some selections are selected from QUERY-REPL::*SELECTION* by name
; a warning is signaled.
#?(query-bind ((test (lambda () (princ :never)))
               (test2 (lambda () (princ :never))))
    (query-eval :test))
:signals warning

; In such case, (values) are returned.
#?(query-bind ((test (lambda () (princ :never)))
               (test2 (lambda () (princ :never))))
    (query-eval :test))
:values ()
,:ignore-signals warning

;;;; Side-Effects:
; Refer *QUERY-EVAL*
; Modify +++ ++ + /// // / *** ** *.

;;;; Notes:

;;;; Exceptional-Situations:

