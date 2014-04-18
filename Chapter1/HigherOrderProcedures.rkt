(define (sum-recursion term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recursion term (next a) next b))))

;; Exercise 1.30

(define (sum-iteration term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))
(define (identity x) x)
(define (cube x) (* x x x))

;; ********************************************************************************

(define (sum-cubes sum a b)
  (sum cube a inc b))

(sum-cubes sum-recursion 1 10)
(sum-cubes sum-iteration 1 10)

;; ********************************************************************************

(define (sum-integers sum a b)
  (sum identity a inc b))

(sum-integers sum-recursion 1 10)
(sum-integers sum-iteration 1 10)

;; ********************************************************************************

(define (integral sum f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral sum-recursion cube 0 1 0.01)
(integral sum-recursion cube 0 1 0.001)
(integral sum-recursion cube 0 1 0.0001)
(integral sum-recursion cube 0 1 0.00001)

(integral sum-iteration cube 0 1 0.01)
(integral sum-iteration cube 0 1 0.001)
(integral sum-iteration cube 0 1 0.0001)
(integral sum-iteration cube 0 1 0.00001)

;; ********************************************************************************

;; Exercise 1.29

(define (integral-using-simpsons-rule sum f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (* (f (+ a (* k h)))
       (cond
         ((= k 0) 1)
         ((= k n) 1)
         ((even? k) 2)
         (else 4))))
  (* (sum term 0 inc n)
     (/ h 3)))

(integral-using-simpsons-rule sum-recursion cube 0.0 1.0 10)
(integral-using-simpsons-rule sum-recursion cube 0.0 1.0 100)
(integral-using-simpsons-rule sum-recursion cube 0.0 1.0 1000)

(integral-using-simpsons-rule sum-iteration cube 0.0 1.0 10)
(integral-using-simpsons-rule sum-iteration cube 0.0 1.0 100)
(integral-using-simpsons-rule sum-iteration cube 0.0 1.0 1000)

;; ********************************************************************************

;; Exercise 1.31

(define (product-recursion term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursion term (next a) next b))))

(define (product-integers p a b)
  (p identity a inc b))

(product-integers product-recursion 1 5)

(define (factorial n)
  (product-integers product-recursion 1 n))

(factorial 5)

(define (compute-pi n)
  (define (round-down n)
    (if (odd? n)
        (- n 1)
        n))
  (define (term n)
    (define numer (+ (round-down n) 2))
    (define denom (+ (round-down (+ n 1)) 1))
    (/ numer denom))
  (* 4
     (product-recursion term 1.0 inc n)))

(compute-pi 10)
(compute-pi 100)
(compute-pi 1000)
