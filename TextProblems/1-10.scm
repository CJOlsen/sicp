
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

What are the values of the following expressions?
(A 1 10)
(A 2 4)
(A 3 3)

;; 1024
;; 65536
;; 65536


Consider the following procedures, where A is the procedure defined
above:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

Give concise mathematical definitions for the functions computed
by the procedures f, g, and h for positive integer values of n. For
example, (k n) computes 5n2.


(define (f n) (A 0 n)) ==> (define (B n)
			     (* 2 y))
                          or f(n) = 2n
        
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

(define (g n) (A 1 n))

(A 1 n)
(cond ((= n 0) 0)
      ((= 1 0) (* 2 y))
      ((= n 1) 2)
      (else (A (- 1 1) (A 1 (- n 1)))))
...
(cond ((= n 0) 0)
      ((= n 1) 2)
      (else (A (- 1 1) (A 1 (- n 1)))))
...
(cond ((= n 0) 0)
      ((= n 1) 2)
      (else (A 0 (A 1 (- n 1)))))
...
(cond ((= n 0) 0)
      ((= n 1) 2)
      (else (A 0 (cond ((= (- n 1) 0) 0)
		       ((= 1 0) (* 2 y))
		       ((= (- n 1) 1) 2)
		       (else (A (- 1 1) (A 1 (- (- n 1) 1))))))))
...
(cond ((= n 0) 0)
      ((= n 1) 2)
      (else (A 0 (cond ((= (- n 1) 0) 0)
		       ((= (- n 1) 1) 2)
		       (else (A 0 (A 1 (- n 2))))))))
...
(cond ((= n 0) 0)
      ((= n 1) 2)
      (else (A 0 (cond ((= (- n 1) 0) 0)
		       ((= (- n 1) 1) 2)
		       (else (A 0 (cond ((= (- n 2) 0) 0)
					((= 1 0) (* 2 (- n 2)))
					((= (- n 2) 1) 2)
					(else (A (- 1 1) (A 1 (- (- n 2) 1)))))))))))
...
(cond ((= n 1) 2)
      (else (A 0 (cond ((= (- n 1) 0) 0)
		       ((= (- n 1) 1) 2)
		       (else (A 0 (cond ((= (- n 2) 0) 0)
					((= 1 0) (* 2 (- n 2)))
					((= (- n 2) 1) 2)
					(else (A 0 (A 1 (- n 3)))))))))))....

;; so this is how g(n) expands with n until the (= y 1) condition is satisfied
;; let's say n=3:

(cond ((= 3 1) 2)
      (else (A 0 (cond ((= (- 3 1) 0) 0)
		       ((= (- 3 1) 1) 2)
		       (else (A 0 (cond ((= (- 3 2) 0) 0)
					((= 1 0) (* 2 (- 3 2)))
					((= (- 3 2) 1) 2)
					(else (A 0 (A 1 (- 3 3)))))))))))

... all of the conditionals drop out and we're left with:
(A 0 (A 0 2)
... we know from earlier that (A 0 n) is 2n   = f(n)
(* 2 (* 2 2))
... so it appears g(n) = 2^n 


(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

(define (h n) (A 2 n))

(A 1 n)
(cond ((= n 0) 0)
      ((= 1 0) (* 2 n))
      ((= n 1) 2)
      (else (A (- 2 1) (A 2 (- n 1)))))
...
(cond ((= n 0) 0)
      ((= 1 0) (* 2 n))
      ((= n 1) 2)
      (else (A (- 2 1) ((cond ((= (- n 1) 0) 0)
			      ((= 2 0) (* 2 y))
			      ((= (- n 1) 1) 2)
			      (else (A (- 2 1) (A 2 (- (- n 1) 1)))))))))
...
(cond ((= n 0) 0)
      ((= 1 0) (* 2 n))
      ((= n 1) 2)
      (else (A 1 ((cond ((= (- n 1) 0) 0)
			      ((= 2 0) (* 2 y))
			      ((= (- n 1) 1) 2)
			      (else (A 1 (A 2 (- n 2)))))))))

so we know from the "(else (A 1 ((..." that the procedure is going to be recursive
with the form g(g(g(z))) but we just don't know what z is yet.

From the tail end we see that the inner-most function will be (A 2 0) which is 0.

g(n) = 2^n

g(g(g(0))) = 2^2^2^1

**** the above proof is pretty "loose" and shouldn't be taken too seriously

to figure out how many two's, we can test n=3

(h 3) ;; 16

(define (square x)
  (* x x))

(h 0) = 0                     = g(0)          = 0^2^0       != 2^0
(h 1) = 2 = 2                 = g(1)          = 2^2^0        = 2^1
(h 2) = 4 = 2^2               = g(g(1))       = 2^2^2^0      = 2^2
(h 3) = 16 = 2^2^2            = g(g(g(1)))    = 2^2^2^2^0    = 2^4
(h 4) = 65536 = 2^2^2^2       = g(g(g(g(1)))) = 2^2^2^2^2^0  = 2^16
(h 5.) = maximum recursion depth exceeded



(f n) = 2n
(g n) = f(f(n) = 2^n
(h n) = (g(g(1)) (with n g's) =  2^2^2^... where there are n 2's


The connection between h and g (from trial and error, unfortunately the proofs above
didn't bring this out):
(h 1) = 2
(g 1) = 2

(h 2) = 4
(g (g 1)) = 4

(h 3) = 16
(g (g (g 1))) = 16

(h 4) = 65536
(g (g (g (g 1)))) = 65536

(g 1) = 2
(f 1) = 2

(g 2)     = 4
(f (f 1)) = 4

(g 3)         = 8
(f (f (f 1))) = 8

(g 4)             = 16
(f (f (f (f 1)))) = 16

So h(n) is a function of n g's with g(1) in the center.
g(n) is a functin of n f's with a f(1) in the center.

It's hard to write down h(n) in terms of f(n) because the number of nested f's isn't
known until you solve an n in a g(n), and every nested g(n) adds a huge number of f's




