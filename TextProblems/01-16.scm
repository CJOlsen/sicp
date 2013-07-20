; Exercise 1-16

; Design a procedure that evolves an iterative exponentiation process that uses
; successive squaring and uses a logarithmic number of steps....


; example, fast-expt
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))


; essentials:
; b^n = (b^(n/2))^2   if n is even
; b^n = b * b^(n-1)   if n is odd
; (b^(n/2))^2 = (b^2)^(n/2) **see note


; choose 'a' to make a*(b^n) an invariant quantity
; explanation:
; every time n is even b gets squared and n gets halved
; every time n is odd 'a' absorbs a b and n is decremented

(define (fast-expt-iter b n)
  (define (expt-helper b n a)
    (cond ((= n 0) a)
	  ((even? n) (expt-helper (square b) (/ n 2) a))
	  (else (expt-helper b (- n 1) (* a b)))))
  (expt-helper b n 1))

(fast-expt-iter 2 10)
;Value: 1024

(fast-expt 2 10)
;Value: 1024


;; **note
(define (fast-expt-iter2 b n)
  (define (expt-helper b n a)
    (cond ((= n 0) a)
	  ((even? n) (square (expt-helper b (/ n 2) a))) ;; <---
	  (else (expt-helper b (- n 1) (* a b)))))
  (expt-helper b n 1))

(fast-expt-iter2 2 10)
;Value: 65536

; explanation: applying square to the expt-helper function itself squares the
;              value a for every time the process recursed, while in the first
;              version the value b is squared and then passed in, allowing the
;              value a to be passed out of the recursive loop unchanged
;              (it's also a different value of 'a' at the end of the recursion)
;              65536 = (((2^2)^2)^2)^2, 1024 = 2*2*2*2*2*2*2*2*2*2