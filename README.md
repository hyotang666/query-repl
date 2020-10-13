# QUERY-REPL 0.0.0
## What is this?
REPL for user query.

## Issue.
Sometimes we need to get user input interactively.
With standard common lisp `restart-case` and `error` is used.
But there is some issues.

* The debugger prints unneeded info for a query.
* The debugger behavior is not portable.

Querying is expected behavior.
It is not exceptional situation.
We should not use `error`.

## Usage
The syntax is almost same with `cl:restart-case` except the first argument.
First argument should print query message.

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
> (+ 1 2 3)

6
  0: [ONE] One!
  1: [TWO] Two!
  2: [INPUT] Invoke restart INPUT.
> 2
Input>> "hoge"
"hoge"
*
```

## Alternatives.
If you does not need full power of REPL.
* [prompt-for](https://github.com/hyotang666/prompt-for)

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
