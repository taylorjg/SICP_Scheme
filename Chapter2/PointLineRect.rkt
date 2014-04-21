;; Exercise 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (print-point (start-segment s))
  (display "->")
  (print-point (end-segment s)))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (make-point
   (average (x-point (start-segment s)) (x-point (end-segment s)))
   (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define p1 (make-point 1 2))
(define p2 (make-point 3 4))
(define s (make-segment p1 p2))
(print-segment s)
(newline)
(print-point (midpoint-segment s))

;; ********************************************************************************

;; Exercise 2.3

; 1st representation of a rectangle: using two points
;(define (make-rect p1 p2)
;  (cons p1 p2))
;
;(define (rect-width r)
;  (abs (- (x-point (car r)) (x-point (cdr r)))))
;
;(define (rect-height r)
;  (abs (- (y-point (car r)) (y-point (cdr r)))))

; 2nd representation of a rectangle: using centre point + width + height
(define (make-rect centre-point w h)
  (cons centre-point (cons w h)))

(define (rect-width r)
  (car (cdr r)))

(define (rect-height r)
  (cdr (cdr r)))

; rect-area and rect-perimeter do not need to change when swapping
; between the two different representations of a rectangle.

(define (rect-area r)
  (* (rect-width r) (rect-height r)))

(define (rect-perimeter r)
  (+ (* 2 (rect-width r))
     (* 2 (rect-height r))))

;(define r1 (make-rect (make-point 0 0) (make-point 5 10)))
(define r2 (make-rect (make-point 2.5 5) 5 10))

