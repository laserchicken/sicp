#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;(1 2 3 4 5 6)

(cons x y)
;(cons (list 1 2 3) (list 4 5 6))
;(cons (list 1 2 3) (cons 4 (const 5 (cons 6 nil))))
;((1 2 3) 4 5 6)

(list x y)
;(cons x (cons y nil))
;(cons (list 1 2 3) (cons (list 4 5 6) nil))
;((1 2 3) (4 5 6))
