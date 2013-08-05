;Exercise 2.8. Using reasoning analogous to Alyssa's, describe how the 
;difference of two intervals may be computed. Define a corresponding 
;subtraction procedure, called sub-interval.

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))



(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define one (make-interval .495 .505))
(define two (make-interval 14.95 15.7))

(sub-interval one two)
;Value: (-15.205 . -14.444999999999999)

(sub-interval two one)
;Value: (14.444999999999999 . 15.205)
