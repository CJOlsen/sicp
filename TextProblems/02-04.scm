;Exercise 2.4
;
; Here is an alternative procedural representation of pairs.  For this 
; representation, verify that (car (cons x y)) yields x for any objects
; x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define a (cons + 567))
(car a)
;Value: #[arity-dispatched-procedure 13]  (heh.)

(define b (cons car cdr))
(car b)
;Value: #[compound-procedure 14 car]

(define c (cons 44 55))
(car c)
;Value: 44

; Substitution model on the last one

(define c (cons 42 55))
(define c (lambda (m) (m 42 55)))
(car c)
(c (lambda (p q) p))
((lambda (m) (m 42 55)) (lambda (p q) p))  ;; wow, p and q were free variables
((lambda (p q) p) 42 55)                   ;; stored in the lambda's closure
42

(define (cdr z)
  (z (lambda (p q) q)))

(define d (cons 42 55))
(car d)  ;;Value: 42
(cdr d)  ;;Value: 55


