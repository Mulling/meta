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
lines starting with `:.` are output. Error handling is very primitive and all state is lost when an error occurs. All input to the REPL must be in a single line.

## Examples:

```lisp
;; currently there is no support for pairs, cons will return a list
.: (cons 3 3)
 :. (3 3)

;; list comparison is supported
.: (eq? (quote (1 2 3)) (cons 1 (cons 2 3)))
 :. 1

;; set is in reality setq, since it does not eval the first argument
(set fib (lambda (x)
           (if (== x 0)
               0
             (if (or (== 1 x) (== 2 x))
                 1
               (+ (fib (- x 1)) (fib (- x 2)))))))

;; set will return the expression, so you can bind a symbol to a lambda and call it at the same time
(set fastfib (lambda (x)
               ((set fib (lambda (old new n)
                           (if (== 0 n)
                               old
                             (fib new (+ old new) (- n 1)))))
                0 1 x)))

;; simple map function
(set map (lambda (f l)
           (if (eq? nil l)
               ()
             (cons (f (car l)) (map f (cdr l))))))

;; in the REPL
.: (map (lambda (x) (+ 1 x)) (cons 3 (cons 4 5)))
 :. (4 5 6)

;; eval will act as do
(if end-of-the-world?
    (eval
     (go-to-bunkers)
     (lunch-ze-missiles))
  (do-nothing))
```
