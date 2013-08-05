; Exercise 2.5
;
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair a and b as the integer
; that is the product (2^a)*(3^b).  Give the corresponding definitions of
; the procedures cons, car, and cdr.


;; I can't find the proof right now, but (2^a)*(3^b) gives only unique results,
;; so to figure out the power of 3 (or 'b'), you can just divide the number by
;; 3 until it will no longer do so evenly.

(define (cons3 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car3 x)
  (define (car3-helper x count)
    (if (= (modulo x 2) 0)
	(car3-helper (/ x 2) (+ count 1))
	count))
  (car3-helper x 0))

(define (cdr3 x)
  (define (cdr3-helper x count)
    (if (= (modulo x 3) 0)
	(cdr3-helper (/ x 3) (+ count 1))
	count))
  (cdr3-helper x 0))

(cons3 3 5) ;1944
(car3 1944) ;3
(cdr3 1944) ;5

(cons3 127 42)
;Value: 18616676303883733237185543291917903883874899983514470449152

(car3 18616676303883733237185543291917903883874899983514470449152)
;Value: 127

(cdr3 18616676303883733237185543291917903883874899983514470449152)
;Value: 42
