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
