;Exercise 1.17

; making a multiplication algorithm using double, halve and addition


; a linear version using only addition
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1))))) ; note we just changed the * operator!


; a logarithmic version using double and halve
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2.))

(define (** a b)
  (cond ((= b 0) 0)
	((even? b) (** (double a) (halve b)))
	(else (+ a (** a (- b 1))))))

;Check:
(** 4 5)
;Value: 20

(** 279 183)
;Value: 51057

(* 279 183)
;Value: 51057
