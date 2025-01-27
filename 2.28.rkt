#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe tree)
  (define (fringe-iter tree rev)
    (cond ((null? tree) rev)
          ((not (pair? (car tree)))
           (fringe-iter (cdr tree) (cons (car tree) rev)))
          (else
           (fringe-iter (cdr tree) (append (fringe-iter (car tree) nil) rev)))))
  (reverse (fringe-iter tree nil)))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
;(1 2 3 4)

(fringe (list x x))
;(1 2 3 4 1 2 3 4)

(define (fringe2 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (list tree))
        (else
         (append (fringe (car tree)) (fringe (cdr tree))))))

(fringe2 x)
;(1 2 3 4)

(fringe2 (list x x))
;(1 2 3 4 1 2 3 4)
