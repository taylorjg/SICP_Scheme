;; http://stackoverflow.com/questions/4521446/creating-an-empty-list-in-racket
(define nil '())

;; Exercise 2.17

(display "exercise 2.17")
(newline)

(define (last-pair items)
  (define (iter a b)
    (if (null? a)
        b
        (iter (cdr a) (cons (car a) nil))))
  (iter items nil))

(last-pair (list 23 72 149 34))
(last-pair nil)

;; ********************************************************************************

;; Exercise 2.18

(display "exercise 2.18")
(newline)

(define (reverse-items items)
  (define (iter a b)
    (if (null? a)
        b
        (iter (cdr a) (cons (car a) b))))
  (iter items nil))

(reverse-items (list 1 4 9 16 25))
(reverse-items nil)

;; ********************************************************************************

;; Exercise 2.20

(display "exercise 2.20")
(newline)

(define (same-parity first . rest)
  (define (iter items result test?)
    (if (null? items)
        result
        (let ((next (car items)))
          (iter
           (cdr items)
           (if (test? next)
               (cons next result)
               result)
           test?))))
  (reverse-items (iter
                  rest
                  (list first)
                  (if (even? first)
                      even?
                      odd?))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; ********************************************************************************

;; Exercise 2.21

(display "exercise 2.21")
(newline)

(define (square x) (* x x))

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (map-jt f items)
  (if (null? items)
      nil
      (cons (f (car items))
            (map-jt f (cdr items)))))

(define (square-list-2 items)
  (map-jt square items))

(square-list-1 (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))

;; ********************************************************************************

;; Exercise 2.23

(display "exercise 2.23")
(newline)

(define (for-each-jt f items)
  (if (not (null? items))
      (let (
            ; no name-expression pairs - we are just using 'let' for its body
            )
        (f (car items))
        (for-each-jt f (cdr items))
        )))

(for-each-jt (lambda (x) (newline)(display x)) (list 57 321 88))

;; ********************************************************************************

;; Exercise 2.24

; (list 1 (list 2 (list 3 4)))
;
; (1 (2 (3 4)))
;
; |.|-|--->|.|/|
;  .        .
; |1|      |.|-|--->|.|/|
;           .        .
;          |2|      |.|-|--->|.|/|
;                    .        .
;                   |3|      |4| 

;; ********************************************************************************

;; Exercise 2.25

(display "exercise 2.25")
(newline)

(define list-1 (list 1 3 (list 5 7) 9))
(define x1 (car (cdr (car (cdr (cdr list-1))))))

(define list-2 (list (list 7)))
(define x2 (car (car list-2)))

(define list-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(define x3 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-3)))))))))))))

;; ********************************************************************************

;; Exercise 2.26

(display "exercise 2.26")
(newline)

(define x (list 1 2 3))
(define y (list 4 5 6))

(newline)
(append x y)
; (1 2 3 4 5 6)

(newline)
; (cons x y)
; ((1 2 3) 4 5 6)

(newline)
(list x y)
; ((1 2 3) (4 5 6))

;; ********************************************************************************

;; Exercise 2.27

(display "exercise 2.27")
(newline)

(define (deep-reverse items)
  (define (iter a b)
    (if (null? a)
        b
        (iter (cdr a) (cons (if (pair? (car a))
                                (deep-reverse (car a))
                                (car a)) b))))
  (iter items nil))

(define deep-list (list (list 1 2) (list 3 4)))

(newline)
(display deep-list)

(newline)
(reverse deep-list)

(newline)
(deep-reverse deep-list)

;; ********************************************************************************

;; Exercise 2.28

(display "exercise 2.28")
(newline)

(define (fringe items)
  (define (iter a result)
    (if (null? a)
        result
        (let ((hd (car a))
              (tl (cdr a)))
          (iter tl (append result (if (pair? hd)
                                      (fringe hd)
                                      (list hd)))))))
  (iter items nil))

(newline)
(fringe deep-list)
(newline)
(fringe (list deep-list deep-list))

;; ********************************************************************************

;; Exercise 2.30

(display "exercise 2.30")
(newline)

(define (square-tree-1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map-jt (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree-2 sub-tree)
                (square sub-tree)))
          tree))

(square-tree-1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; ********************************************************************************

;; Exercise 2.31

(display "exercise 2.31")
(newline)

(define (tree-map f tree)
  (map-jt (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map f sub-tree)
                (f sub-tree)))
          tree))

(define (square-tree-3 tree) (tree-map square  tree))

(square-tree-3 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; ********************************************************************************

;; Exercise 2.32

(display "exercise 2.32")
(newline)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map-jt
                      (lambda (x) (cons (car s) x))
                      rest)))))

(subsets (list 1 2 3))

;; ********************************************************************************

;; Exercise 2.33

(display "exercise 2.33")
(newline)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (map-jt-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map-jt-2 square (list 2 4 6))

(define (append-jt seq1 seq2)
  (accumulate cons seq2 seq1))

(append-jt (list 1 2 3) (list 4 5 6))

(define (length-jt sequence)
  (accumulate (lambda (_ y) (+ y 1)) 0 sequence))

(length-jt (list 1 2 3 4 5 6 7 8 9 10))

;; ********************************************************************************

;; Exercise 2.34

(display "exercise 2.34")
(newline)

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; ********************************************************************************

;; Exercise 2.35

(display "exercise 2.35")
(newline)

(define (count-leaves-1 x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-1 (car x))
                 (count-leaves-1 (cdr x))))))

(display deep-list)
(newline)
(count-leaves-1 deep-list)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves-2 tree)
  (accumulate + 0 (map (lambda (_) 1) (enumerate-tree tree))))

(display deep-list)
(newline)
(count-leaves-2 deep-list)

;; ********************************************************************************

;; Exercise 2.36

(display "exercise 2.36")
(newline)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(newline)
(display seqs)

(newline)
(accumulate-n + 0 seqs)

;; ********************************************************************************

;; Exercise 2.37

(display "exercise 2.36")
(newline)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v1 (list 1 2 3))
(define v2 (list 4 5 6))
; result: 32
(dot-product v1 v2)

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

;; http://mathinsight.org/matrix_vector_multiplication_examples
(define m-1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define v-1 (list -2 1 0))
; result: (0 -3 -6 -9)
(matrix-*-vector m-1 v-1)

(define (transpose m)
  (accumulate-n cons nil m))

(define m-2 (list (list 1 2) (list 3 4) (list 5 6)))
; result: ((1 3 5) (2 4 6))
(transpose m-2)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col) (dot-product m-row n-col))
                cols))
         m)))

;; http://www.intmath.com/matrices-determinants/matrix-multiplication-examples.php
(define m-a (list (list -4 2 -3) (list 1 3 4)))
(define m-b (list (list -1 -2 2 0) (list 5 1 6 3) (list 4 7 8 9)))
; result: ((2 -11 -20 -21) (30 29 52 45))
(matrix-*-matrix m-a m-b)

;; ********************************************************************************

