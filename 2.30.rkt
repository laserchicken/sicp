#lang sicp

#|
(define (map proc list)
  (if (null? list)
      nil
      (cons (proc (car list)) (map proc (cdr list)))))

(map (lambda (x) (* x x)) (list 1 2 3 4))
|#

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
