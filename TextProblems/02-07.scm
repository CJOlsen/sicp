;Exercise 2.7. Alyssa's program is incomplete because she has not specified the implementation of the
;interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

;Define selectors upper-bound and lower-bound to complete the implementation.


(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define interval (make-interval .495 .505))

(lower-bound interval)
;Value: .495

(upper-bound interval)
;Value: .505