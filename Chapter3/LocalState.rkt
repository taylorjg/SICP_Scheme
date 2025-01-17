;; http://stackoverflow.com/questions/9500068/random-function-in-drracket
(#%require (only racket/base random))
(#%require (only racket/base random-seed))
(#%require (only racket/base error))

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
              (else (error "Unknown request -- MAKE-ACCOUNT-2" m)))
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

;; Exercise 3.4

(define (call-the-cops)
  (display "THE COPS ARE ON THE WAY!"))

(define (make-account-3 balance password)
  (let ((bad-password-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)))
    (define (dispatch password-attempt m)
      (if (eq? password-attempt password)
          (cond ((eq? m 'withdraw) (begin (set! bad-password-count 0) withdraw))
                ((eq? m 'deposit) (begin (set! bad-password-count 0) deposit))
                (else (error "Unknown request -- MAKE-ACCOUNT-3" m)))
          (lambda (_) (begin
                        (set! bad-password-count (+ bad-password-count 1))
                        (if (>= bad-password-count 7)
                            (call-the-cops)
                            "Incorrect password")))))
    dispatch))

(define acc-3 (make-account-3 100 'p455w0rd))
((acc-3 'p455w0rd 'withdraw) 30)
((acc-3 'p455w0rd 'withdraw) 45)
((acc-3 'p455w0rd 'deposit) 15)
((acc-3 'p455w0rd 'withdraw) 0)
((acc-3 'p455w0rd 'withdraw) 50)
((acc-3 'bogus 'deposit) 10)
((acc-3 'bogus 'deposit) 10)
((acc-3 'bogus 'deposit) 10)
((acc-3 'bogus 'deposit) 10)
((acc-3 'bogus 'deposit) 10)
((acc-3 'bogus 'deposit) 10)
((acc-3 'bogus 'deposit) 10)
(newline)

;; --------------------------------------------------------------------------------

;; Exercise 3.5

(define (rand)
  (random 1000000000))

;(define (random-in-range low high)
;  (let ((range (- high low)))
;    (+ low (random range))))

(define (random-in-range low high)
  (let ((range (- high low))
        (random-0-to-1 (random)))
    (+ low (* random-0-to-1 range))))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1)(+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(display "estimate-pi")
(newline)
(estimate-pi 100)
(estimate-pi 1000)
(estimate-pi 10000)
(estimate-pi 100000)
(estimate-pi 1000000)

(define (estimate-integral predicate pt1 pt2 trials)
  (let* ((x1 (car pt1))
         (y1 (cdr pt1))
         (x2 (car pt2))
         (y2 (cdr pt2))
         (area (* (- x2 x1) (- y2 y1))))
    (* area (monte-carlo
             trials
             (lambda () (predicate pt1 pt2))))))

(define (point-in-region-test pt1 pt2)
  (let* ((x1 (car pt1))
         (y1 (cdr pt1))
         (x2 (car pt2))
         (y2 (cdr pt2))
         (rx (/ (- x2 x1) 2))
         (ry (/ (- y2 y1) 2))
         (cx (+ x1 rx))
         (cy (+ y1 ry))
         (x (random-in-range x1 x2))
         (y (random-in-range y1 y2)))
    (<= (+ (* (- x cx) (- x cx)) (* (- y cy) (- y cy))) (* rx rx))))

(display "estimate-integral")
(newline)
(estimate-integral point-in-region-test (cons -1.0 -1.0) (cons 1.0 1.0) 100)
(estimate-integral point-in-region-test (cons -1.0 -1.0) (cons 1.0 1.0) 1000)
(estimate-integral point-in-region-test (cons -1.0 -1.0) (cons 1.0 1.0) 10000)
(estimate-integral point-in-region-test (cons -1.0 -1.0) (cons 1.0 1.0) 100000)
(estimate-integral point-in-region-test (cons -1.0 -1.0) (cons 1.0 1.0) 1000000)

;; --------------------------------------------------------------------------------

;; Exercise 3.6

(define (rand-jt m)
  (cond ((eq? m 'generate) (random 11))
        ((eq? m 'reset) random-seed)
        (else (error "Unknown request -- RAND-JT" m))))

(define (dump-some-random-numbers n)
  (define (iter counter)
    (if (> counter 0)
        (begin
          (newline)
          (display "resetting random...")
          ((rand-jt 'reset) 999)
          (newline)
          (display (rand-jt 'generate))
          (newline)
          (display (rand-jt 'generate))
          (newline)
          (display (rand-jt 'generate))
          (iter (- counter 1)))))
  (iter n))

(dump-some-random-numbers 3)

;; --------------------------------------------------------------------------------

;; Exercise 3.7

(define (make-joint acc initial-password new-password)
  (define (dispatch new-password-attempt m)
    (if (eq? new-password-attempt new-password)
        (acc initial-password m)
        (lambda (_) "Incorrect new password")))
  dispatch)

(define peter-acc (make-account-2 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(newline)
((peter-acc 'open-sesame 'withdraw) 10)
((peter-acc 'open-sesame 'withdraw) 20)
((paul-acc 'open-sesame 'withdraw) 5)
((paul-acc 'rosebud 'withdraw) 5)

;; --------------------------------------------------------------------------------

;; Exercise 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define list-x (list 'a 'b))
(define list-y (list 'c 'd))
(define list-z (append list-x list-y))
(cdr list-x) ; (b)
(define list-w (append! list-x list-y))
(cdr list-x) ; (b, c, d)

;; --------------------------------------------------------------------------------

;; Exercise 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define infinite-list (make-cycle (list 1 2 3))) ; #0=(1 2 3 . #0#)
; (last-pair infinite-list) ; would run forever!

;; --------------------------------------------------------------------------------

;; Exercise 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define list-in (list 1 2 3 4))
list-in
(mystery list-in)

;; --------------------------------------------------------------------------------
