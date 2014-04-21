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
  (define (iter a b test?)
    (if (null? a)
        b
        (let ((next (car a)))
          (iter
           (cdr a)
           (if (test? next)
               (cons next b)
               b)
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

