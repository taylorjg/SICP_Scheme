;; http://stackoverflow.com/questions/4521446/creating-an-empty-list-in-racket
(define nil '())

;; Exercise 2.17

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

(define list-1 (list 1 3 (list 5 7) 9))
(define x1 (car (cdr (car (cdr (cdr list-1))))))

(define list-2 (list (list 7)))
(define x2 (car (car list-2)))

(define list-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(define x3 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-3)))))))))))))

;; ********************************************************************************

;; Exercise 2.26

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

;; ********************************************************************************
