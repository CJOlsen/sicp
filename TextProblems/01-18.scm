; Exercise 1-18

; make an iterative logarithmic multiplication algorithm that only uses
; double, halve and addtion 


(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

(define (* a b)
  (define (*-helper a b x)
    (cond ((= b 0) x)
	  ((even? a) (*-helper (halve a) (double b) x))
	  (else (*-helper a (- b 1) (+ x a)))))
  (*-helper a b 0))

(* 4 5)
;Value: 20

;; The above seems strange at first glance, but closer inspections shows it
;; to be an implementation of Russian Peasant Multiplication, which is 
;; what the footnote in the book says it should be
