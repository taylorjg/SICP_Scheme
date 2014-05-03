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

;; Exercise 3.3

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define (make-account-2 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch password-attempt m)
    (if (eq? password-attempt password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (lambda (_) "Incorrect password")))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 30)
((acc 'withdraw) 45)
((acc 'deposit) 15)
((acc 'withdraw) 0)
((acc 'withdraw) 50)

(define acc-2 (make-account-2 100 'p455w0rd))
((acc-2 'p455w0rd 'withdraw) 30)
((acc-2 'p455w0rd 'withdraw) 45)
((acc-2 'p455w0rd 'deposit) 15)
((acc-2 'p455w0rd 'withdraw) 0)
((acc-2 'p455w0rd 'withdraw) 50)
((acc-2 'bogus 'withdraw) 10)
((acc-2 'bogus 'deposit) 10)

;; --------------------------------------------------------------------------------
