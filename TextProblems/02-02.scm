; Exercise 2.2
;
; make make-segment, start-segment, end-segment, make-point, x-point, y-point


(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (midpoint-segment segment)
  (let ((a (start-segment segment))
	(b (end-segment segment)))
    (let ((new-x (/ (+ (x-point a) (x-point b)) 2.))
	  (new-y (/ (+ (y-point a) (y-point b)) 2.)))
      (make-point new-x new-y))))

(define a (make-point 0 0))
(define b (make-point 9 9))
(define c (make-segment a b))
(define midpoint (midpoint-segment c))

(print-point midpoint)
;Value: (4.5,4.5)