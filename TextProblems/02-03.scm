; Exercise 2.3
;
; make a two representations of a rectangle with perimeter and area procedures
; that will work on either version


(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rectangle first-corner far-corner)
  (cons 'corners (cons first-corner far-corner)))
(define (first-corner rectangle)
  (if (eq? (car rectangle) 'corners)
      (cadr rectangle)
      #f))
(define (far-corner rectangle)
  (if (eq? (car rectangle) 'corners)
      (cddr rectangle)
      #f))

(define (perimeter-1 rectangle)
  (+ (* 2 (- (x-point (far-corner rectangle))
	     (x-point (first-corner rectangle))))
     (* 2 (- (y-point (far-corner rectangle))
	     (y-point (first-corner rectangle))))))
(define (area-1 rectangle)
  (* (- (x-point (far-corner rectangle)) (x-point (first-corner rectangle)))
     (- (y-point (far-corner rectangle)) (y-point (first-corner rectangle)))))

(define (make-rectangle2 corner height width)
  (cons 'height-width (cons corner (cons height width))))
(define (height rectangle)
  (if (eq? (car rectangle) 'height-width)
      (caddr rectangle)
      #f))
(define (width rectangle)
  (if (eq? (car rectangle) 'height-width)
      (cdddr rectangle)
      #f))

(define (perimeter-2 rectangle)
  (+ (* 2 (height rectangle)) (* 2 (width rectangle))))
(define (area-2 rectangle)
  (* (height rectangle) (width rectangle)))

(define (perimeter rectangle)
  (cond ((eq? (car rectangle) 'corners)
	 (perimeter-1 rectangle))
	((eq? (car rectangle) 'height-width)
	 (perimeter-2 rectangle))
	(else 'ERROR)))

(define (area rectangle)
  (cond ((eq? (car rectangle) 'corners)
	 (area-1 rectangle))
	((eq? (car rectangle) 'height-width)
	 (area-2 rectangle))
	(else 'ERROR)))

;; Testing

(define a (make-rectangle (make-point 0 0) (make-point 8 8)))
(perimeter a) ;Value: 32
(area a)      ;Value: 64

(define b (make-rectangle2 (make-point 0 0) 8 8))
(perimeter b) ;Value: 32
(area b)      ;Value: 64

(define c (make-rectangle (make-point 143 199) (make-point 543 42)))
(perimeter c) ;Value: 486
(area c)      ;Value: -62800

(define d (make-rectangle2 (make-point 143 199) 400 -157))
(perimeter d) ;Value: 486
(area d)      ;Value: -62800


;; Notes:
;; 1) This isn't a full (as in well-done) typing system
;; 2) This should probably be modified to handle negative numbers properly
;; 3) make-rectangle could take as arguments a point and something else
;;    and then decide which rectangle representation to use 
;;    like this....

(define (make-rect point . args)
  (if (null? (cdr args))
      (make-rectangle point (car args))
      (make-rectangle2 point (car args) (cadr args))))

(define e (make-rect (make-point 0 0) (make-point 8 8)))
(perimeter e) ;Value: 32
(area e)      ;Value: 64

(define f (make-rect (make-point 0 0) 8 8))
(perimeter f) ;Value: 32
(area f)      ;Value: 64

	 
		