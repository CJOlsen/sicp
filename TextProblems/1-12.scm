Exercise 1.12: The following pattern of numbers is called Pascal\u2019s
triangle.
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
. . .
The numbers at the edge of the triangle are all 1, and each number
inside the triangle is the sum of the two numbers above it. Write a procedure that computes elements of Pascal's triangle by means of a recursive process.


(index-ref (list 1 2 3 4 5) 3)

(define (pascal row)
  (define (sub-pascal current-list index len)
    (cond ((= index 0)
	   (sub-pascal (append current-list (list 1))
		       (+ index 1)
		       len))
	  ((= index len)
	   (append current-list (list 1)))
	  (else
	   (sub-pascal (append current-list
			       (list (+ (list-ref row index)
					(list-ref row (- index 1)))))
		       (+ index 1)
		       len))))
  (sub-pascal () 0 (length row)))

(pascal (list 1 3 3 1))

(define (make-triangle size)
  (define (tri-helper count size last-row)
    (newline)
    (display last-row)
    (if (= count size)
	#t
	(tri-helper (+ count 1) size (pascal last-row))))
  (tri-helper 1 size (list 1)))


(make-triangle 9)

(1)
(1 1)
(1 2 1)
(1 3 3 1)
(1 4 6 4 1)
(1 5 10 10 5 1)
(1 6 15 20 15 6 1)
(1 7 21 35 35 21 7 1)
(1 8 28 56 70 56 28 8 1)
(1 9 36 84 126 126 84 36 9 1)
