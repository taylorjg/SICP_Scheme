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
    (if (and (> remaining 0) (not (stream-null? s)))
        (begin
          (display-element (stream-car s))
          (iter (stream-cdr s) (- remaining 1)))))
  (iter s n)
  (newline))

(define (display-element x)
  (display x)
  (display " "))

(define (show x)
  (display-element x)
  x)

(define (show-named-stream name stream)
  (define (display-element-with-name element)
    (display "<")
    (display name)
    (display ": ")
    (display element)
    (display "> ")
    (newline)
    element)
  (stream-map display-element-with-name stream))

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

;; Infinite Streams

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

;; Sieve of Eratosthenes

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(display-stream-n primes 20)

;; --------------------------------------------------------------------------------

;; Defining streams implicitly

(define ones (cons-stream 1 ones))
(display-stream-n ones 10)

(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers2 (cons-stream 1 (add-streams ones integers2)))
;(define integers2 (cons-stream 1 (show-named-stream "add" (add-streams (show-named-stream "ones" ones) (show-named-stream "integers2" integers2)))))
(display-stream-n integers2 10)

(define fibs2 (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(display-stream-n fibs2 20)

;; --------------------------------------------------------------------------------

(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(display-stream-n double 10)

(define primes2 (cons-stream 2 (stream-filter prime2? (integers-starting-from 3))))
(define (prime2? n)
  (define (square x) (* x x))
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes2))
(display-stream-n primes2 20)

;; --------------------------------------------------------------------------------

;; Exercise 3.54

(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams integers2 factorials)))
(display-stream-n factorials 10)

;; --------------------------------------------------------------------------------

;; Exercise 3.55

;; 1  | 2  3  4  5  
;;    | 1  3  6  10
;; ----------------
;; 1    3  6  10 15

(define (partial-sums s) (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))
(display-stream-n (partial-sums integers2) 10)

;; --------------------------------------------------------------------------------

;; Exercise 3.59a

(define (integrate-series coefficients) (stream-map / coefficients integers2))
(display-stream-n (integrate-series ones) 10)

;; Exercise 3.59b

(define exp-series (cons-stream 1 (integrate-series exp-series)))
(display-stream-n exp-series 10)

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map
                                    (lambda (coeff n)
                                      (cond ((odd? n) 0)
                                            ((divisible? n 4) coeff)
                                            (else (- coeff))))
                                    exp-series
                                    integers2))))
(display-stream-n cosine-series 10)

(define sine-series
  (cons-stream 0 (integrate-series (stream-map
                                    (lambda (coeff n)
                                      (cond ((even? n) 0)
                                            ((divisible? (+ n 1) 4) (- coeff))
                                            (else coeff)))
                                    exp-series
                                    integers2))))
(display-stream-n sine-series 10)

(define (negate-stream s) (stream-map - s))
(define cosine-series-2 (cons-stream 1 (negate-stream (integrate-series sine-series-2))))
(define sine-series-2 (cons-stream 0 (integrate-series cosine-series-2)))
(display-stream-n cosine-series-2 10)
(display-stream-n sine-series-2 10)

;; --------------------------------------------------------------------------------
