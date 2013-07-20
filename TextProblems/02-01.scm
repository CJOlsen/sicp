; Exercise 2-1
; Generalize make-rat to handle negative numbers

(define (xor a b)
  (cond ((and (not a) (not b))
	 #f)
	((and (not a) b)
	 #t)
	((and a (not b))
	 #t)
	((and a b)
	 #f)))

(define (make-rat n d)
  (let ((neg-factor (if (xor (< n 0) (< d 0))
			-1
			1))
	(abs-n (abs n))
	(abs-d (abs d))
	(g (gcd (abs n) (abs d))))
    (cons (* (/ abs-n g) neg-factor)
	  (/ abs-d g))))



(make-rat 2 4)
;Value: (1 . 2)


(make-rat 2 -4)
;Value: (-1 . 2)


(make-rat -3 -12)
;Value: (1 . 4)


(make-rat -3 15)
;Value: (-1 . 5)
