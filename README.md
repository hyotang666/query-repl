# QUERY-REPL 4.0.9
## What is this?
REPL for user query.

## Issue.
Sometimes we need to get user input interactively.
With standard common lisp `y-or-n-p`, `yes-or-no-p`, or `restart-case` with `invoke-restart` is used.
But there are some issues.

* `y-or-n-p` and `yes-or-no-p` is less powerful.
* The debugger prints unneeded info for a query.
* The debugger behavior is not portable.

Querying is expected behavior.
It is not an exceptional situation.
We should not use debugger.

## Usage
For details, see [spec file.](spec/query-repl.lisp)
### `QUERY-CASE`
The syntax is almost same with `cl:restart-case` except the first argument and :test keyword param.
First argument should print query message.
The return value of the first argument is discarded.

```lisp
* (query-case (write-line "Choose one of below." *query-io*)
    (one () :report "One!" 1)
    (two () :report "Two!" 2)
    (input (input) :interactive (lambda () (format *query-io* "Input>> ")
                                  (force-output *query-io*)
                                  (list (read)))
      input))
Choose one of below.

  0: [ONE  ] One!
  1: [TWO  ] Two!
  2: [INPUT] Invoke restart INPUT.

;; You can do anything in REPL.
> (+ 1 2 3)

6

;; To choose restart, type integer or restart-name.
  0: [ONE] One!
  1: [TWO] Two!
  2: [INPUT] Invoke restart INPUT.
> 2
Input>> "hoge"
"hoge"
*
```
### `*QUERY-EVAL*`
If you do not need the full power of REPL, you can bind `*QUERY-EVAL*` with `NIL`.
In such cases, `QUERY-REPL` acts as echo REPL, and `CL:*READ-EVAL*` is bound by `NIL` automatically.

```lisp
* (let (*query-eval*)
    (query-case (write-line "Works like echo." *query-io*)
      (one () :report "One" 1)))
Works like echo.

  0: [ONE] One
> (+ 1 2 3)

(+ 1 2 3)
  0: [ONE] One

;; Reader error is ignored.
> #.(+ 1 2 3)
WARNING:
   Ignore: can't read #. while *READ-EVAL* is NIL

             Stream: #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10000385B3}>

NIL
  0: [ONE] One
> )
WARNING:
   Ignore: unmatched close parenthesis

             Stream: #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10000385B3}>

NIL
  0: [ONE] One
> no-such-package:symbol
WARNING:
   Ignore: Package NO-SUCH-PACKAGE does not exist.

             Stream: #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10000385B3}>

NIL
  0: [ONE] One

;; Ctl-C is also ignored.
> WARNING: Ignore: Interactive interrupt at #x7F31187D3CF9.

NIL
  0: [ONE] One

;; Ctl-D is also ignored.
> WARNING:
   Ignore: end of file on #<SB-SYS:FD-STREAM for "the terminal" {1001578A53}>

NIL
  0: [ONE] One

> 0
1
```
### `SELECT`
When selection is dynamic values, you can use `SELECT`.

```lisp
* (select (list 1 'symbol #'car))

  0: [SELECT] 1
  1: [SELECT] SYMBOL
  2: [SELECT] #<FUNCTION CAR>
> 2
#<FUNCTION CAR>
```

### `PAGED-SELECT`
If selection is big, `PAGED-SELECT` may helps you.

```lisp
* (paged-select (loop :for i :below 20 :collect i))

  0: [SELECT] #:NEXT
  1: [SELECT] 0
  2: [SELECT] 1
  3: [SELECT] 2
  4: [SELECT] 3
  5: [SELECT] 4
  6: [SELECT] 5
  7: [SELECT] 6
  8: [SELECT] 7
  9: [SELECT] 8
> 0

  0: [SELECT] #:NEXT
  1: [SELECT] #:PREV
  2: [SELECT] 9
  3: [SELECT] 10
  4: [SELECT] 11
  5: [SELECT] 12
  6: [SELECT] 13
  7: [SELECT] 14
  8: [SELECT] 15
  9: [SELECT] 16
> 5
12
*
```
#### &KEY MAX
To specify max selection num, use the keyword parameter `:MAX` (the default is 10).

##### NOTE
The selection includes `NEXT` and/or `PREV`.

```lisp
* (paged-select (loop :for i :below 20 :collect i)
                :max 5)

  0: [SELECT] #:NEXT
  1: [SELECT] 0
  2: [SELECT] 1
  3: [SELECT] 2
  4: [SELECT] 3
>
```
#### &KEY KEY
When keyword parameter `:KEY` is specified, such function is applied to each element of selection.

##### NOTE
The `:KEY` function is applied only to printed selection.
If the user selects from the first page, the rest elements are never applied.

```lisp
* (paged-select (loop :for i :below 20 :collect i)
                :key (lambda (x) (princ x) (force-output) (princ-to-string x)))
012345678               ; <--- The function applied only first page elements.
  0: [SELECT] #:NEXT
  1: [SELECT] "0"
  2: [SELECT] "1"
  3: [SELECT] "2"
  4: [SELECT] "3"
  5: [SELECT] "4"
  6: [SELECT] "5"
  7: [SELECT] "6"
  8: [SELECT] "7"
  9: [SELECT] "8"
> 0                     ; <--- Choose next page.
910111213141516         ; <--- The function applied only this page elements.
  0: [SELECT] #:NEXT
  1: [SELECT] #:PREV
  2: [SELECT] "9"
  3: [SELECT] "10"
  4: [SELECT] "11"
  5: [SELECT] "12"
  6: [SELECT] "13"
  7: [SELECT] "14"
  8: [SELECT] "15"
  9: [SELECT] "16"
> 1                     ; <--- Choose previous page.
                        ; <--- The function is not applied.
  0: [SELECT] #:NEXT
  1: [SELECT] "0"
  2: [SELECT] "1"
  3: [SELECT] "2"
  4: [SELECT] "3"
  5: [SELECT] "4"
  6: [SELECT] "5"
  7: [SELECT] "6"
  8: [SELECT] "7"
  9: [SELECT] "8"
> 0

  0: [SELECT] #:NEXT
  1: [SELECT] #:PREV
  2: [SELECT] "9"
  3: [SELECT] "10"
  4: [SELECT] "11"
  5: [SELECT] "12"
  6: [SELECT] "13"
  7: [SELECT] "14"
  8: [SELECT] "15"
  9: [SELECT] "16"
> 0
171819
  0: [SELECT] #:PREV
  1: [SELECT] "17"
  2: [SELECT] "18"
  3: [SELECT] "19"
> 1

"17"
*
```

## From developer

### Product's goal
?

### License
MIT

### Developed with
SBCL

### Tested with
* SBCL/2.0.10
* CCL/1.12
* ECL/20.4.24
* CLISP/2.49

## Installation
TODO
