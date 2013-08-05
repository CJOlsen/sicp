;;Exercise 2.17. Define a procedure last-pair that returns the list that contains only the last element of a
;;given (nonempty) list:
;;(last-pair (list 23 72 149 34))
;;(34)


(define (last-pair the-list)
  (if (null? (cdr the-list))
      the-list
      (last-pair (cdr the-list))))

(last-pair (list 23 72 149 34))
;Value: (34)

