;Exercise 2.9. The width of an interval is half of the difference between 
;its upper and lower bounds. The width is a measure of the uncertainty of 
;the number specified by the interval. For some arithmetic operations the 
;width of the result of combining two intervals is a function only of the 
;widths of the argument intervals, whereas for others the width of the 
;combination is not a function of the widths of the argument intervals. Show 
;that the width of the sum (or difference) of two intervals is a function 
;only of the widths of the intervals being added (or subtracted). Give 
;examples to show that this is not true for multiplication or division.


(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))
(define (width-interval x)
  (- (upper-bound x) (lower-bound x)))


(define a (make-interval 2.71 3.14))
(define b (make-interval 271.23 314.99))
(define c (make-interval 1.23 44.99))

(= (width-interval (sub-interval a b))
   (width-interval (sub-interval a c)))
;Value: #t

(= (width-interval (mul-interval a b))
   (width-interval (mul-interval a c)))
;Value: #f


(= (width-interval (div-interval a b))
   (width-interval (div-interval a c)))
;Value: #f

