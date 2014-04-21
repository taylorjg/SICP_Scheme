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
