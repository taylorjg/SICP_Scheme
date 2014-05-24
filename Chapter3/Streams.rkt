;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html#eval-order_streams_title_1
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define the-empty-stream '())
(define (stream-null? s) (null? s))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map f s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (f (stream-car s)) (stream-map f (stream-cdr s)))))

(define (stream-for-each f s)
  (if (not (stream-null? s))
      (begin
        (f (stream-car s))
        (stream-for-each f (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s)) (cons-stream (stream-car s) (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (display-stream s)
  (begin
    (stream-for-each display-element s)
    (newline)))

(define (display-stream-n s n)
  (define (iter s remaining)
    (if (> remaining 0)
        (begin
          (display-element (stream-car s))
          (iter (stream-cdr s) (- remaining 1)))))
  (begin
    (iter s n)
    (newline)))

(define (display-element x)
  (display x)
  (display " "))

;; --------------------------------------------------------------------------------

;; Exercise 3.50

(define (stream-map f . list-of-streams)
  (if (null? (car list-of-streams))
      the-empty-stream
      (cons-stream
       (apply f (map stream-car list-of-streams))
       (apply stream-map (cons f (map stream-cdr list-of-streams))))))

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 4 (cons-stream 5 (cons-stream 6 the-empty-stream))))
(define s3 (cons-stream 7 (cons-stream 8 (cons-stream 9 the-empty-stream))))
(display-stream (stream-map + s1 s2 s3))
(display-stream-n (stream-map + s1 s2 s3) 2)

;; --------------------------------------------------------------------------------

(define (integers-starting-from n) (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7))) integers))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(display-stream-n (integers-starting-from 10) 10)
(display-stream-n integers 10)
(display-stream-n no-sevens 20)
(display-stream-n fibs 20)

;; --------------------------------------------------------------------------------



;; --------------------------------------------------------------------------------
