Exercise 1.8: Newton\u2019s method for cube roots is based on the fact
that if y is an approximation to the cube root of x, then a better
approximation is given by the value

x/(y^2) + 2y
------------
      3
.
Use this formula to implement a cube-root procedure analogous
to the square-root procedure. (In Section 1.3.4 we will see how to
implement Newton\u2019s method in general as an abstraction of these
square-root and cube-root procedures.)


(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))



(define (improve guess x)
  (* (average guess (/ x guess)) 1.0))



(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))


(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))


(define (cube-rt x)
  (cube-iter 1. x))

(cube-rt 27)  ;; 3.000


;; or with a different style of good-enough? based on guess reaching a steady
;; state

(define (good-enough2? guess last-guess)
  (< (abs (- guess last-guess)) (* .01 guess)))

(define (cube-iter2 guess x last-guess)
  (if (good-enough2? guess last-guess)
      guess
      (cube-iter2 (improve guess x) x guess)))

(define (cube-rt2 x)
  (cube-iter2 1. x 0))

(cube-rt2 8)  ;; 2.000




