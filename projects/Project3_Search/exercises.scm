;;Project 3 Exercises

;; 1. Write BFS-Simple based on DFS-Simple


(define (DFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))

(define (BFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append old new))
	  graph))

(BFS-simple 'a
	    (lambda (node) (eq? node 'l))
	    test-graph)

;; reversing old and new in the merge lambda function leaves the 
;; old list (the parent node's children) where it is and appends
;; the new nodes to be searched afterwards.  This simple change
;; is the difference between a depth first and a vreadth first
;; search!


;; 2. Marking visited nodes (write search-with-cycles)


(define (add-to-list a-list new-value)
  (cons a-list (list new-value)))

(define (in-list? a-list value)
  (if (null? a-list)
      #f
      (if (eq? (car a-list) value)
	  #t
	  (in-list? (cdr a-list) value))))
	

;; the new search-with-cycles
(define (search-with-cycles initial-state goal? successors merge graph)
  (define (search-inner still-to-do visited-nodes)
    (if (or (null? still-to-do)
	    (in-list? visited-nodes (car still-to-do)))
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do))
	       (add-to-list visited-nodes current))))))
  (search-inner (list initial-state) (list)))

;; DFS with cycles
(define (DFS-with-cycles start goal? graph)
  (search-with-cycles start
		      goal?
		      find-node-children
		      (lambda (new old) (append old new))
		      graph))

;; test DFS-with-cycles
(DFS-with-cycles 'a
		 (lambda (node) (eq? node 'l))
		 test-graph)


;; Depth First Order:

(now-at a)
(now-at b)
(now-at c)
(now-at d)
(now-at e)
(now-at f)
(now-at g)
(now-at h)
(now-at i)
(now-at j)
(now-at k)
(now-at l)
(now-at m)
;Value: #f


;; Breadth First Order

(now-at a)
(now-at b)
(now-at i)
(now-at m)
(now-at c)
(now-at d)
(now-at e)
(now-at h)
(now-at j)
(now-at k)
(now-at l)
(now-at f)
(now-at g)
;Value: #f


;; test on the-web
(DFS-with-cycles 'http://sicp.csail.mit.edu/
		 (lambda (node) (eq? node 'zzz))
		 the-web)

(visited-nodes: ())
(now-at http://sicp.csail.mit.edu/)
(visited-nodes: (http://sicp.csail.mit.edu/))
(now-at http://sicp.csail.mit.edu/schemeimplementations)
(visited-nodes: (http://sicp.csail.mit.edu/ http://sicp.csail.mit.edu/schemeimplementations))
(now-at http://sicp.csail.mit.edu/getting-help.html)
(visited-nodes: (http://sicp.csail.mit.edu/ http://sicp.csail.mit.edu/schemeimplementations http://sicp.csail.mit.edu/getting-help.html))
;Value: #f



;; Exercise 3: The Index Abstraction


;; TO BE IMPLEMENTED
;;(define (add-to-index! index key value) ; Index,Key,Val -> Index
;;  (let ((index-entry (find-entry-in-index index key)))
;;    (if (null? index-entry)
;;      ;; no entry -- create and insert a new one...
;;	;... TO BE IMPLEMENTED
;;
;;      ;; entry exists -- insert value if not already there...
;;	;... TO BE IMPLEMENTED
;;	))
;;  index)

;; Index Data-Structure:
;  ('index . (list (list key
;		         (list value1
;			       value2))
;		   (list key
;		         (list value))))

(define (member? a-list value)
  (if (null? a-list)
      #f
      (if (equal? (car a-list) value)
	  #t
	  (member? (cdr a-list) value))))
	  
(define (add-to-index! index key value) ; Index,Key,Val -> Index
  (let ((index-entry (find-entry-in-index index key)))
    (if (null? index-entry)
	(set-cdr! index (cons (list key
				    (list value))
			      (cdr index)))
	(if (member? (cadr index-entry) value)
	    (write-line 'value_already_exists)
	    (set-cdr! index (cons (list key
					(cons value
					      (cadr index-entry)))
				  (cdr index)))))))

;; Testing
(define test-index (make-index))
(add-to-index! test-index 'key1 'value1)
(add-to-index! test-index 'key2 'value2)
(add-to-index! test-index 'key1 'another-value1)
(add-to-index! test-index 'key1 'and-another-1)
(add-to-index! test-index 'key3 'value3)

(find-in-index test-index 'key1)
(find-in-index test-index 'key2)

test-index


Exercise 4: A Web Index

(define the-web-index (make-index))

(define (add-document-to-index! index web url)
  (define (add-helper word-list)
    (if (null? word-list)
	#t
	(let ((word (car word-list)))
	  (add-to-index! index word url)
	  (add-helper (cdr word-list)))))
  (add-helper (find-URL-text web url)))


;; Example use
;; 
 (define the-web-index (make-index))
 
 (add-document-to-index! the-web-index
                         the-web
                         'http://sicp.csail.mit.edu/)

 
(find-in-index the-web-index 'help)
;Value: (http://sicp.csail.mit.edu/)

(find-in-index the-web-index '*magic*)
;Value: #f
    ;; This test returns () instead of #f, but it looks intentional from the 
    ;; find-in-index definition, so it's gonna slide for now



;; Exercise 5: Crawling the Web to Build a Web Index

;; New search function

;; the new search-with-cycles-and-procedure
(define (search-with-cycles-and-procedure initial-state goal? successors
					  merge graph node-procedure index)
  (define (search-inner still-to-do visited-nodes)
    (write-line (list 'visited-nodes: visited-nodes))
    (if (or (null? still-to-do)
	    (in-list? visited-nodes (car still-to-do)))
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (node-procedure index graph current)
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do))
	       (add-to-list visited-nodes current))))))
  (search-inner (list initial-state) (list)))

(define (BFS-web start goal? graph node-procedure index)
  (search-with-cycles-and-procedure start
				    goal?
				    find-node-children
				    (lambda (new old) (append new old))
				    graph
				    node-procedure
				    index))

(define (make-web-index web start)
  (let ((new-index (make-index)))
    (bfs-web start
	     (lambda (x) #f) ;; goal that always returns false
	     the-web
	     add-document-to-index!
	     new-index) ;; this mutates the new-index data structure
    (lambda (word)
      (find-in-index new-index word))))

(define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))

(find-documents 'collaborative)
;Value: (http://sicp.csail.mit.edu/)
