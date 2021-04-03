# Meta

## About:

`meta` is a toy LISP interpreter. The goal for the language is to combine features from Lisp 1 and Lisp 2
and to allow runtime reader macros. This project also includes a small
parser combinator 'library' written as an applicative parser.

The implementation is done in Haskell and doesn't use any external library. As a "challenge" this is written without a single `do`.

Project for the [Lambda PET 2021](https://github.com/petcomp-unioeste/lambda-pet/tree/Lambda2021), MAROMBAPET🄯 edition.

## Using:

To compile use:

`$ ghc --make Main`

or use the provided `Makefile`

`$ make`

Inside the REPL you can use `,list` to list all of the available symbols. It's pretty self explanatory. Lines stating with `.:` are input and
lines starting with `:.` are output.

## Examples:

```lisp
;; set is in reality setq, since it does not eval the first argument
(set fib (lambda (x)
           (if (or (== 0 x) (== 1 x))
               1
             (+ (fib (- x 1)) (fib (- x 1))))))


(set fat (labmda (x)
           (if (== 0 x)
               1
             (* x (fat (- x 1))))))

;; set will return the expression, so you can bind a symbol to a lambda and call it at the same time
(set fastfib (lambda (x)
               ((set fib (lambda (old new n)
                  (if (== 0 n)
                      new
                    (fib new (+ old new) (- n 1)))))
                0 1 x)))
```
