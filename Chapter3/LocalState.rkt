;; --------------------------------------------------------------------------------

;; Exercise 3.1

(define (make-accumulator initial)
  (let ((acc initial))
    (lambda (x)
      (begin (set! acc (+ acc x)) acc))))

(define A (make-accumulator 5))
(A 10)
(A 10)

;; --------------------------------------------------------------------------------

;; Exercise 3.2

(define (make-monitored f)
  (let ((count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1)) (f m))))
      )
    dispatch)
  )

(define mf (make-monitored sqrt))
(mf 100)
(mf 'how-many-calls?)
(mf 49)
(mf 'how-many-calls?)
(mf 'reset-count)
(mf 'how-many-calls?)
(mf 81)
(mf 'how-many-calls?)

;; --------------------------------------------------------------------------------
