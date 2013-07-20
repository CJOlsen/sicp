;;; SEARCH.SCM
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3

(define *search-debug* #t)         ; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object not element: " element)
      (first (cdr element))))

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))

;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? graph)                  ; anytype -> boolean
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object not a graph: " graph)
      (cdr graph)))

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
	#f
	(graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '())))

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '())))

;; Testing...

(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'a '(b c) '(words for node a))
   (make-graph-element 'b '(c) '(words for node b))
   (make-graph-element 'c '(a) '(words for node c)))))

; (find-graph-element test-graph 'b)
; (find-graph-element test-graph 'z)
; (find-node-children test-graph 'b)
; (find-node-children test-graph 'z)
; (find-node-contents test-graph 'b)
; (find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do)))))))
  (search-inner (list initial-state)))



(define (add-to-list a-list new-value)
  (append a-list (list new-value)))


(add-to-list (list 'a 'b 'c) 'd)


(define (in-list? a-list value)
  (if (null? a-list)
      #f
      (if (eq? (car a-list) value)
	  #t
	  (in-list? (cdr a-list) value))))

(in-list? (list) (list))

;; the new search-with-cycles
(define (search-with-cycles initial-state goal? successors merge graph)
  ;; initial-state: start node
  ;; successors: computes successor states from current state
  ;; merge: combnines new states with current states to explore
  ;; graph: the graphs being searched (the-web for this problem)
  (define (search-inner still-to-do visited-nodes)
    (write-line (list 'visited-nodes: visited-nodes))
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
		      (lambda (new old) (append new old))
		      graph))

;; test DFS-with-cycles
(DFS-with-cycles 'a
		 (lambda (node) (eq? node 'zzz))
		 test-graph)

;; test on the-web
(DFS-with-cycles 'http://sicp.csail.mit.edu/
		 (lambda (node) (eq? node 'zzz))
		 the-web)

;(BFS-simple 'http://sicp.csail.mit.edu/
;	    (lambda (node) (eq? node 'zzz))
;	    the-web)
		 

(define (DFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))

;; Exercise 1
(define (BFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append old new))
	  graph))

(BFS-simple 'a
	    (lambda (node) (eq? node 'zzz))
	    test-graph)

; (DFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)
  

;; you will need to write a similar search procedure that handles cycles

;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a 
;; list of Index-Entries.  Each Index-Entry associates
;; a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;; 

(define (make-index)            ; void -> Index
  (list 'index))

(define (index? index)          ; antype -> boolean
  (and (pair? index) (eq? 'index (car index))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set-cdr! index '())
              index)))
      
; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry)
        '())))

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


;;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation 
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol      

; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))


;; The real definition of THE-WEB we'll use is in another file, 
;; including all of the words in the documents.

;;(define the-web
;;  (list
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/
;;    '(http://sicp.csail.mit.edu/SchemeImplementations/
;;      http://sicp.csail.mit.edu/projects/)
;;    '(... words extracted from http://sicp.csail.mit.edu/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/projects/
;;    '(http://sicp.csail.mit.edu/collaborative-work.html
;;      http://sicp.csail.mit.edu/getting-help.html)
;;    '(... words extracted from http://sicp.csail.mit.edu/SchemeImplementations/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/getting-help.html
;;    '(http://sicp.csail.mit.edu/
;;      http://sicp.csail.mit.edu/SchemeImplementations/)
;;    '(... words extracted from http://sicp.csail.mit.edu/getting-help.html))
;;   ...))


;;--------------------
;; Searching the Web
;; you need to write expressions to search the web using different search
;; strategies


;;--------------------
;; Indexing the Web
;;
;;   Our purpose in creating an index of a web is to
;; later support the ability to find any pages that contain
;; a given word.  Thus, a Key in our index will be a Word,
;; and the values in the index will be the URLs of pages
;; that contain that word.

;; A procedure to help  with indexing web pages
;; using the Index abstraction.  The idea is to
;; get the text associated with the URL from the
;; web, and then key each of the words in the
;; text into the index.

;; TO BE IMPLEMENTED
;; add-document-to-index!: Index, Web, URL
;; (define (add-document-to-index! index web url)
;; ...
;; )

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
(add-document-to-index! the-web-index
			the-web
			'http://sicp.csail.mit.edu/getting-help.html)
(length the-web-index)
(find-in-index the-web-index 'help)
;returns both urls from above

(find-in-index the-web-index '*magic*)
;Value: #f
    ;; This test returns () instead of #f, but it looks intentional from the 
    ;; find-in-index definition, so it's gonna slide for now



;; Exercise 5: Crawling the Web to Build a Web Index

;; **** this may need to be rewritten ****

;; the new search-with-cycles-and-procedure
(define (search-w-cycles-procedure-and-index2 initial-state goal? successors
					     merge graph node-procedure index)
  (define (search-inner still-to-do visited-nodes index)
    (if (or (null? still-to-do)
	    (in-list? visited-nodes (car still-to-do)))
	#f
	(let ((current-node (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current-node)))
	  (node-procedure index graph current-node)
	  (write-line (list 'search-inner_index_length: (length index)))
	  (if (goal? current-node)
	      #t
	      (search-inner
	       (merge (successors graph current-node) (cdr still-to-do))
	       (add-to-list visited-nodes current-node)
	       index)))))
  (search-inner (list initial-state) (list) index))

(define (BFS-web start goal? graph node-procedure index)
  (search-w-cycles-procedure-and-index2 start
				       goal?
				       find-node-children
				       (lambda (new old) (append new old))
				       graph
				       node-procedure
				       index))
(define (make-web-index web start)
  (let ((new-index (make-index)))
    (bfs-web start
	     (lambda (x) #f) ;; always returns false
	     the-web
	     add-document-to-index!
	     new-index) ;; this mutates the new-index data structure
    (write-line (list 'make-web-index_index_length: (length new-index)))
    (lambda (word)
      (find-in-index new-index word))))

(define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))

(find-documents 'collaborative)
;Value: (http://sicp.csail.mit.edu/)

(find-documents 'or)
;Value: (http://sicp.csail.mit.edu/getting-help.html http://sicp.csail.mit.edu/schemeimplementations)


(define (search-w-cycles-procedure-and-index-one initial-state goal? successors
					     merge graph node-procedure index)
  (define (search-inner still-to-do visited-nodes index)
    (if (or (null? still-to-do)
	    (in-list? visited-nodes (car still-to-do)))
	#f
	(let ((current-node (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current-node)))
	  (node-procedure index graph current-node)
	  (if (goal? current-node)
	      current-node
	      (search-inner
	       (merge (successors graph current-node) (cdr still-to-do))
	       (add-to-list visited-nodes current-node)
	       index)))))
  (search-inner (list initial-state) (list) index))



;Execise 6: A Dynamic Web Search

;; this searches the entire graph and returns *all* matches

(define (search-with-cycles-all2 initial-state goal? successors merge graph)
  ;; this turned out to be a bit of a mess to write
  ;;  successors: function to find new nodes
  ;;  merge: function to combine to-do list
  ;;  goal? will recieve 2 arguments, the graph and current-node
  (define (search-inner2 still-to-do visited-nodes successes)
    ;; still-to-do: a list of nodes to visit (including current)
    ;; visited-nodes: a list of visited nodes
    ;; successes: a list of successful matches
    (if (null? still-to-do)
	successes
	(let ((the-current-node (car still-to-do)))
	  (cond ((member? visited-nodes the-current-node)
		 successes)
		((goal? graph the-current-node)
		 (search-inner2 (merge (successors graph the-current-node)
				       (cdr still-to-do))
				(add-to-list visited-nodes the-current-node)
				(append (list the-current-node) successes)))
		(else
		 (search-inner2 (merge (successors graph the-current-node)
				       (cdr still-to-do))
				(add-to-list visited-nodes the-current-node)
				successes))))))
  (search-inner2 (list initial-state) () ()))
			  
(define (search-all web start-node word)
  (search-with-cycles-all2 start-node
			   (lambda (graph current-node)
			     (let ((y (find-graph-element graph current-node)))
			       (if (null? y)
				   #f
				   (member? (graph-element->contents y)
					    word))))
			   find-node-children
			   (lambda (new old) (append new old))
			   web))

(search-all the-web 'http://sicp.csail.mit.edu/ 'collaborative)
;Value: (http://sicp.csail.mit.edu/)

(search-all the-web 'http://sicp.csail.mit.edu/ 'or)
;Value: (http://sicp.csail.mit.edu/getting-help.html http://sicp.csail.mit.edu/schemeimplementations)


(define (search-with-cycles-any initial-state goal? successors merge graph)
  ;; this turned out to be a bit of a mess to write
  ;;  successors: function to find new nodes
  ;;  merge: function to combine to-do list
  ;;  goal? will recieve 2 arguments, the graph and current-node
  (define (search-inner still-to-do visited-nodes)
    ;; still-to-do: a list of nodes to visit (including current)
    ;; visited-nodes: a list of visited nodes
    ;;(define the-current-node (car still-to-do))
    (if (null? still-to-do)
	#f
	(let ((the-current-node (car still-to-do)))
	  (cond ((member? visited-nodes the-current-node)
		 #f)
		((goal? graph the-current-node)
		 the-current-node)
		(else
		 (search-inner (merge (successors graph
						  the-current-node)
				      (cdr still-to-do))
			       (add-to-list visited-nodes
					    the-current-node)))))))
  (search-inner (list initial-state) ()))

(define (search-any web start-node word)
  (search-with-cycles-any start-node
			  (lambda (graph current-node)
			    (let ((y (find-graph-element graph current-node)))
			      (if (null? y)
				  #f
				  (member? (graph-element->contents y)
					   word))))
			  find-node-children
			  (lambda (new old) (append new old))
			  web))


(search-any the-web 'http://sicp.csail.mit.edu/ 'collaborative)
;Value: http://sicp.csail.mit.edu/

(search-any the-web 'http://sicp.csail.mit.edu/ 'or)
;Value: http://sicp.csail.mit.edu/schemeimplementations

;;;; These all match the earlier search implementation



;; ****
;; **** Computer Exercise 7: Comparison - Web Index vs. Dynamic Search
;; ****

;; n = 10
(define first-web (generate-random-web 10))
(define first-page (caaddr (cadr first-web)))

(timed search-any first-web first-page 'help)
time expended: .00999999999999801

(timed search-any first-web first-page 'Susanhockfield)
;time expended: 0.

(timed search-all first-web first-page 'help)
;time expended: 0.

(define find-documents-1 (timed make-web-index first-web first-page))
;time expended: 0.

(timed find-documents-1 'help)
;time expended: 0.

(timed find-documents-1 'Susanhockfield)
;time expended: 0.

;; n = 50
(define second-web (generate-random-web 50))
(define second-page (caaddr (cadr first-web)))

(timed search-any second-web second-page 'help) ;; really?  'help not found?
;time expended: 0.

(timed search-any second-web second-page 'Susanhockfield)
;time expended: 0.

(timed search-all second-web second-page 'help)
;time expended: 0.

(define find-documents-2 (timed make-web-index second-web second-page))
;time expended: 0.

(timed find-documents-2 'help)
;time expended: 0.

(timed find-documents-2 'Susanhockfield)
;time expended: 0.


;; n = 199
(define third-web (generate-random-web 199))
(define third-page (graph-root first-web))

(timed search-any third-web third-page 'help)
;time expended: 0.

(timed search-any third-web third-page 'Susanhockfield)
;time expended: .03999999999999204

(timed search-all third-web third-page 'help)
;time expended: .03999999999999204

(define find-documents-3 (timed make-web-index third-web third-page))
;time expended: 0.

(timed find-documents-3 'help)
;time expended: 0. ;; but it didn't find anything :/

(timed find-documents-3 'Susanhockfield)
;time expended: 0.





;;------------------------------------------------------------
;; utility for timing procedure calls.
;; returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))


(timed list . '1)


;;
;; Exercise 8: Using A Better Indexing Scheme
;;



