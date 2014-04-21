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
