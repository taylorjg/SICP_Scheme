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

(define (stream-map f s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (f (stream-car s)) (stream-map f (stream-cdr s)))))

(define (stream-for-each f s)
  (if (not (stream-null? s))
      (begin
        (f (stream-car s))
        (stream-for-each f (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define s1 (cons-stream 2 (cons-stream 3 the-empty-stream)))
(define s2 (stream-map (lambda (x) (* x x)) s1))
