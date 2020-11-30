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

(requirements-about QUERY-CASE :doc-type function)

;;;; Description:

#+syntax (QUERY-CASE query &body clauses) ; => result

;;;; Arguments and Values:

; query := Form which print query message.

; clause := (name lambda-list query-param* &body body)
; name := symbol, otherwise error.
#?(query-case () ("not symbol" ())) :signals error
; Not evaluated.
#?(query-case () ((intern "Not evaluated"))) :signals error
; lambda-list := ordinary lambda list.
#?(query-case () (name "not list")) :signals error
; query-param := [ :report report-form | :interactive function-name ]
; report-form := [ function-name | string ]
; When report-form function-name is specified, such function should have API as (function (stream)).
#?(query-case () (name () :report (formatter "not function name"))) :signals error
; When string is specified, treated as (lambda (s) (format s string)).
#?(with-input-from-string (in "0")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (query-case () (name () :report "Printed."))))
:outputs "
  0: [NAME] Printed.
> "
,:stream *query-io*
; function-name := [ symbol | (cons (eql lambda)) ]
; When interactive function is specified, such function should have API as (function () list).
; Returned values are applied to LAMBDA-LIST and BODY is evaluated.
#?(with-input-from-string (in "0")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (query-case ()
        (name (a) :interactive (lambda () (list :returned))
          a))))
=> :RETURNED
,:stream nil
; body := implicit progn.

; result := T

;;;; Affected By:
; QUERY-REPL::*SELECTIONS* internally.
#?(with-input-from-string (in "test")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (block :block
        (query-bind ((test (lambda () (return-from :block :return))))
          (query-case ()
            (a () :a))))))
:outputs "
  0: [A   ] A
  1: [TEST] TEST
> "
,:stream *query-io*

#?(with-input-from-string (in "test")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (block :block
        (query-bind ((test (lambda () (return-from :block :return))))
          (query-case ()
            (a () :a))))))
=> :RETURN
,:stream nil

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SELECT :doc-type function)

;;;; Description:

#+syntax (SELECT list) ; => result

;;;; Arguments and Values:

; list := list, otherwise error.
#?(select :atom) :signals type-error

; result := selected elt of the list.
#?(with-input-from-string (in "0") ; <--- select 0 th.
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (select '(1 2 3))))
=> 1
,:stream nil

#?(with-input-from-string (in "1") ; <--- select 1 th.
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (select '(1 2 3))))
=> 2
,:stream nil

;;;; Affected By:
; Dynamic environment of QUERY-REPL::*SELECTIONS*
#?(with-input-from-string (in "1")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (block :block
        (query-bind ((test (lambda () (return-from :block :selected))))
          (select '(:never))))))
=> :SELECTED
,:stream nil

;;;; Side-Effects:
; Input / Output *query-io*
#?(with-input-from-string (in "1")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (block :block
        (query-bind ((test (lambda () (return-from :block :selected))))
          (select '(:never))))))
:outputs "
  0: [SELECT] :NEVER
  1: [TEST  ] TEST
> "
,:stream *query-io*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PAGED-SELECT :doc-type function)

;;;; Description:

#+syntax (PAGED-SELECT list &key (max 10) (key #'identity)) ; => result

;;;; Arguments and Values:

; list := list, otherwise an implementation dependent condition.
#?(paged-select "not list") :signals condition

; max := (integer 3 *), otherwise an implementation dependent condition.
#?(paged-select '(1 2 3) :max 1) :signals condition
#?(paged-select '(1 2 3) :max -1) :signals condition
#?(paged-select '(1 2 3) :max 10.0) :signals condition
#?(paged-select '(1 2 3) :max "not integer") :signals condition
; When specified, output selection has specified size.
#?(with-input-from-string (in "1")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (paged-select (loop :for i :below 10 :collect i) :max 3)))
:outputs "
  0: [SELECT] #:NEXT
  1: [SELECT] 0
  2: [SELECT] 1
> "
,:stream *query-io*

; key := function designator, otherwise an implementation dependent condition.
#?(paged-select '(1 2 3) :key "not function") :signals condition
; If specified, each element is applied.
#?(with-input-from-string (in "1")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (paged-select '(1 2 3) :key #'princ-to-string)))
=> "2"
,:test equal
,:stream nil

; result := T

;;;; Affected By:
; Dynamic environment of QUERY-REPL::*SELECTIONS*
#?(with-input-from-string (in "3")
    (let ((*query-io* (make-two-way-stream in *query-io*)))
      (block :block
        (query-bind ((test (lambda () (return-from :block :returned))))
          (paged-select '(1 2 3))))))
=> :RETURNED
,:stream nil

; *QUERY-EVAL*
#?(with-input-from-string (in "(print :hoge) 0")
    (let ((*query-io* (make-two-way-stream in (make-broadcast-stream))))
      (paged-select '(1 2 3))))
:outputs "
:HOGE "

;;;; Side-Effects:

;;;; Notes:
; Key function is applied only printed page elements.
#?(with-input-from-string (in "1")
    (let ((*query-io* (make-two-way-stream in (make-broadcast-stream))))
      (paged-select (loop :for i :below 20 :collect i)
                    :key (lambda (x) (princ x) (force-output) x))))
:outputs "012345678"

; Additionaly such function is applied only once.
#?(with-input-from-string (in (concatenate 'string
                                           "0" ; <--- Choose #:NEXT.
                                           " 1" ; <--- Choose #:PREV.
                                           " 2")) ; <--- Select.
    (let ((*query-io* (make-two-way-stream in (make-broadcast-stream))))
      (paged-select (loop :for i :below 20 :collect i)
                    :key (lambda (x) (princ x) (force-output) x))))
:outputs "012345678910111213141516"

;;;; Exceptional-Situations:

(requirements-about <MAKE-SELECTION-FORM> :doc-type function :test equal)

;;;; Description:

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

