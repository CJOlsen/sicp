Exercise 1.11: A function f is defined by the rule that 
f (n) = n if n < 3 
and 
f (n) = f (n - 1) + 2f (n - 2) + 3f (n - 3) if n >= 3. 

Write a
procedure that computes f by means of a recursive process. Write
a procedure that computes f by means of an iterative process.


Iterative definition:

(define (hip n)
  (define (hip-helper count n a b c)
    (if (= count n)
	(+ c (* 2 b) (* 3 a))
	(hip-helper (+ count 1)
		    n
		    b
		    c
		    (+ c (* 2 b) (* 3 a)))))
  (if (< n 3)
      n
      (hip-helper 3 n 0 1 2)))


Recursive Definition:

(define (hop n)
  (if (< n 3)
      n
      (+ (hop (- n 1))
	 (* 2 (hop (- n 2)))
	 (* 3 (hop (- n 3))))))

    
    