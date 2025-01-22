#lang sicp

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds)


(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((pair? tree) 1)
        (else
         (+ (count-leaves (car tree))
            (count-leaves (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)

(list x x)
(count-leaves (list x x))
      
