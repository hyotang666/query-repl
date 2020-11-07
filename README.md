# QUERY-REPL 0.0.0
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
The syntax is almost same with `cl:restart-case` except the first argument.
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
In such cases, `QUERY-REPL` acts as echo REPL.

```lisp
* (let (*query-eval*)
    (query-case (write-line "Works like echo." *query-io*)
      (one () :report "One" 1)))
Works like echo.

  0: [ONE] One
> (+ 1 2 3)

(+ 1 2 3)
  0: [ONE] One
> 0
1
```
*NOTE*

Still read time evaluation works.
You may need to bind `CL:*READ-EVAL*` with `NIL`.

## From developer

### Product's goal
?

### License
MIT

### Developed with
SBCL

### Tested with

## Installation
TODO
