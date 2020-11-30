(defpackage :query-repl.spec
  (:use :cl :jingoh :query-repl)
  (:import-from :query-repl #:<make-selection-form>))
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

(requirements-about QUERY-BIND :doc-type function)

;;;; Description:

#+syntax (QUERY-BIND (&rest bind*) &body body) ; => result

;;;; Arguments and Values:

; bind := (query-name function &key report-function interactive-function)
; query-name := symbol, otherwise an implementation dependent condition.
#?(query-bind (("not symbol" (lambda () :dummy)))) :signals condition
; Not evaluated.
#?(query-bind (((intern "Not evaluated") (lambda () :dummy)))) :signals condition
; function := function, otherwise an implementation dependent condition.
#?(query-bind ((name "not function"))) :signals condition
#?(query-bind ((name 'no-such-function))) :signals condition
; Evaluated.
#?(query-bind ((name (coerce 'list 'function)))) :invokes-debugger not
; If the argument FUNCTION does not transfer control flow, EXP is returned.
#?(query-bind ((name #'list))
    (query-eval 0))
=> 0
; report-function := function, otherwise an implementation dependent conditioin.
#?(query-bind ((name #'list :report-function "not function"))) :signals condition
; Evaluated.
#?(query-bind ((name #'list :report-function (coerce #'print 'function))))
:invokes-debugger not
; If specified such function should have API as (function (stream)).
; Specified function is called to print selection report.
#?(query-bind ((name (lambda () :dummy)
                 :report-function (lambda (s) (format s "Report"))))
    (query-prompt))
:outputs "
  0: [NAME] Report
> "
,:stream *query-io*
; When NIL (the default), selection name is printed as report.
#?(query-bind ((name (lambda () :dummy)))
    (query-prompt))
:outputs "
  0: [NAME] NAME
> "
,:stream *query-io*
; interactive-function := function, otherwise an implementation dependent condition.
#?(query-bind ((name (lambda () :dummy) :interactive-function "not function")))
:signals condition
; If specifed, such function should have API as (function () list).
; Specifed function is called when selection is selected.
#?(query-bind ((name (lambda (a) a)
                 :interactive-function (lambda () (list (princ :hoge)))))
    (query-eval 0))
:outputs "HOGE"
; If specified, such functions return value is APPLYed the first argument FUNCTION.
#?(query-bind ((name #'princ :interactive-function (lambda () (list :a *standard-output*))))
    (query-eval 0))
:outputs "A"

; body := implicit PROGN

; result := T.

;;;; Affected By:
; QUERY-REPL::*SELECTIONS*

;;;; Side-Effects:
; Modify environment i.e. QUERY-REPL::*SELECTIONS*.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about QUERY-REPL :doc-type function)

;;;; Description:

#+syntax (QUERY-REPL) ; => result

;;;; Arguments and Values:

; result := T

;;;; Affected By:
; QUERY-REPL::*SELECTIONS*
#?(block :block
         (query-bind ((nil (lambda () (return-from :block 0))))
           (with-input-from-string (s "0")
             (let ((*query-io* (make-two-way-stream s *query-io*)))
               (query-repl)))))
=> 0
,:stream nil

; *QUERY-EVAL*
; When *QUERY-EVAL* is NIL, and meed #. dispatch macro
; warning is signaled.
#?(let (*query-eval*)
    (block :block
      (query-bind ((nil (lambda () (return-from :block 0))))
        (with-input-from-string (s "#.1")
          (let ((*query-io* (make-two-way-stream s *query-io*)))
            (query-repl))))))
:signals warning
; In such case, do next loop.
#?(let (*query-eval*)
    (block :block
      (query-bind ((nil (lambda () (return-from :block 0))))
        (with-input-from-string (s "#.1 0")
          (let ((*query-io* (make-two-way-stream s *query-io*)))
            (query-repl))))))
=> 0
,:stream nil
,:ignore-signals warning

; *READ-EVAL* *READ-BASE* *READ-DEFAULT-FLOAT-FORMAT* *READ-SUPPRESS*

;;;; Side-Effects:
; io *QUERY-IO*.

;;;; Notes:

;;;; Exceptional-Situations:

#+syntax (<MAKE-SELECTION-FORM> clause block) ; => result

;;;; Arguments and Values:

; clause := 

; block := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests.
#?(<make-selection-form> '(name ()) :block)
=> (NAME (LAMBDA () (RETURN-FROM :BLOCK (PROGN)))
         :REPORT-FUNCTION (LAMBDA (query-repl::S) (FORMAT query-repl::S "~A" 'NAME))
         :INTERACTIVE-FUNCTION NIL)

#?(<make-selection-form> '(name () :report fun-name-as-symbol) :block)
=> (NAME (LAMBDA () (RETURN-FROM :BLOCK (PROGN)))
         :REPORT-FUNCTION #'FUN-NAME-AS-SYMBOL
         :INTERACTIVE-FUNCTION NIL)
#?(<make-selection-form> '(name () :report (lambda (s) (format s "hoge"))) :block)
=> (NAME (LAMBDA () (RETURN-FROM :BLOCK (PROGN)))
         :REPORT-FUNCTION #'(LAMBDA (S) (FORMAT S "hoge"))
         :INTERACTIVE-FUNCTION NIL)
#?(<make-selection-form> '(name () :report "hoge") :block)
=> (NAME (LAMBDA () (RETURN-FROM :BLOCK (PROGN)))
         :REPORT-FUNCTION (LAMBDA (query-repl::S) (FORMAT query-repl::S "hoge"))
         :INTERACTIVE-FUNCTION NIL)
#?(<make-selection-form> '(name () :interactive (lambda () (list 0))) :block)
=> (NAME (LAMBDA () (RETURN-FROM :BLOCK (PROGN)))
         :REPORT-FUNCTION (LAMBDA (QUERY-REPL::S) (FORMAT QUERY-REPL::S "~A" 'NAME))
         :INTERACTIVE-FUNCTION #'(LAMBDA () (LIST 0)))
#?(<make-selection-form> '(name () :interactive) :block)
=> (NAME (LAMBDA () (RETURN-FROM :BLOCK (PROGN :INTERACTIVE)))
         :REPORT-FUNCTION (LAMBDA (QUERY-REPL::S) (FORMAT QUERY-REPL::S "~A" 'NAME))
         :INTERACTIVE-FUNCTION NIL)

