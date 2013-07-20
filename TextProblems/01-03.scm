Exercise 1.3: Define a procedure that takes three numbers as arguments
and returns the sum of the squares of the two larger numbers.


(define (sum-square-bigs a b c)
  (define (sum-squares x y)
    (+ (* x x) (* y y)))
  (cond ((= a b)
	 (if (> a c)
	     (sum-squares a b)
	     (sum-squares a c)))
	((= a c)
	 (if (> a b)
	     (sum-squares a c)
	     (sum-squares a b)))
	((= b c)
	 (if (> b a)
	     (sum-squares b c)
	     (sum-squares b a)))
	((and (< a b) (< a c))
	 (sum-squares b c))
	((and (< b a) (< b c))
	 (sum-squares a c))
	((and (< c a) (< c b))
	 (sum-squares a b))))


(sum-square-bigs 1 2 3) ;; 13
(sum-square-bigs 1 2 2) ;; 8
(sum-square-bigs 1 1 1) ;; 2
(sum-square-bigs 5 5 6) ;; 61


	      