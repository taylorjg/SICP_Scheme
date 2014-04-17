(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (identity x) x)
(define (cube x) (* x x x))

;; ********************************************************************************

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

;; ********************************************************************************

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

;; ********************************************************************************

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)
(integral cube 0 1 0.00001)

;; ********************************************************************************

;; Exercise 1.29

(define (integral-using-simpsons-rule f a b n)
  (define (helper h)
    (define (term k)
      (* (f (+ a (* k h)))
         (cond
           ((= k 0) 1)
           ((= k n) 1)
           ((even? k) 2)
           (else 4))))
    (* (sum term 0 inc n)
       (/ h 3)))
  (helper (/ (- b a) n)))

(integral-using-simpsons-rule cube 0.0 1.0 10)
(integral-using-simpsons-rule cube 0.0 1.0 100)
(integral-using-simpsons-rule cube 0.0 1.0 1000)

;; ********************************************************************************

;; Exercise 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        0
        (iter ?? ??)))
    (iter a 0))

