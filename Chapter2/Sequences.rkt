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

