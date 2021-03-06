Exercise 1.7: The good-enough? test used in computing square
roots will not be very effective for finding the square roots of very
small numbers. Also, in real computers, arithmetic operations are
almost always performed with limited precision. This makes our
test inadequate for very large numbers. Explain these statements,
with examples showing how the test fails for small and large numbers.
An alternative strategy for implementing good-enough? is to
watch how guess changes from one iteration to the next and to
stop when the change is a very small fraction of the guess. Design
a square-root procedure that uses this kind of end test. Does this
work better for small and large numbers?



(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define q 123456789012345678901234567890123456789012345678901234567891)
(define r 123456789012345678901234567890123456789012345678901234567891)

(good-enough? q r) ;; returns False!

(define m .00000000000000000000000005)
(define n .00000000000000000000000010)

(good-enough? m n) ;; returns True even thogh n = 2*m


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt1 x)
  (sqrt-iter 1 x))


;; an alternative good-enough? based on the size of guess

(define (good-enough-2? guess last-guess)
  (< (abs (- guess last-guess)) (* .1 guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (* (average guess (/ x guess)) 1.0))

(define (my-sqrt x)
  (define (sqrt-iter guess x last-guess)
    (if (good-enough-2? guess last-guess)
	guess
	(sqrt-iter (improve guess x) x guess)))
  (sqrt-iter 1 x 0))

;;(my-sqrt 9)

(sqrt1 .00001)    ;; .00314
(my-sqrt .00001) ;; .00317 


(sqrt1 .000000000001)    ;; .03125
(my-sqrt .000000000001) ;; 1.0005e-6

(sqrt1 12345678901234567890123456789012345678901234567890123456789)
    ;; forever loop

(my-sqrt 12345678901234567890123456789012345678901234567890123456789)
    ;;  1.111e29


;; my-sqrt terminates more quickly that sqrt1 for large numbers, (but maybe
;; because improve uses a (* 1.0) factor to convert to decimal representation)

;; my-sqrt gives a better answer than sqrt1 on small numbers where sqrt1's
;; good-enough? function lacks the needed granularity for a precise answer