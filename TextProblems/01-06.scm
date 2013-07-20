;;Exercise 1.6: Alyssa P. Hacker doesn\u2019t see why if needs to be provided
;;as a special form. \u201cWhy can\u2019t I just define it as an ordinary
;;procedure in terms of cond?\u201d she asks. Alyssa\u2019s friend Eva Lu Ator
;;claims this can indeed be done, and she defines a new version of
;;if:

;;(define (new-if predicate then-clause else-clause)
;;  (cond (predicate then-clause)
;;	(else else-clause)))

;;Eva demonstrates the program for Alyssa:
;;(new-if (= 2 3) 0 5)
;;5
;;(new-if (= 1 1) 0 5)
;;0

;;Delighted, Alyssa uses new-if to rewrite the square-root program:
;;(define (sqrt-iter guess x)
;;  (new-if (good-enough? guess x)
;;	  guess
;;	  (sqrt-iter (improve guess x) x)))

;;What happens when Alyssa attempts to use this to compute square
;;roots? Explain.



;;This is definitely strange.  The procedures get evaluated as they're passed
;;in to new-if, so it doesn't depend on the conditional whether or not each one
;;is evaluated.  Here's a simpler example:

(define (prints-everything x y)
  #f)

(prints-everything (display "World!") (display "Hello "))

;; This returns #f, but in the *scheme* buffer we see "Hello World!"
;; even though the procedure itself does nothing with the x and y that are
;; passed to it.

;; Also interesting is the order they are evaluated, from right to left.