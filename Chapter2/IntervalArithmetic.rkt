;; Exercise 2.7

(define (make-interval l u)
  (cons l u))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))
  
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; ********************************************************************************

;; Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; ********************************************************************************

;; Exercise 2.9

(define (interval-width x)
  (- (upper-bound x) (lower-bound x)))

(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display "-")
  (display (upper-bound x))
  (display "]")
  (display " w: ")
  (display (interval-width x)))

(define i1 (make-interval 10 12))
(define i2 (make-interval 2 6))
(print-interval i1)
(print-interval i2)
(print-interval (add-interval i1 i2))
(print-interval (sub-interval i1 i2))
(print-interval (mul-interval i1 i2))
(print-interval (div-interval i1 i2))

;; ********************************************************************************
