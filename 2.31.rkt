#lang sicp

(define (square x) (* x x))

(define (tree-map-1 proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map-1 proc (car tree))
                    (tree-map-1 proc (cdr tree))))))

(define (square-tree-1 tree) (tree-map-1 square tree))

(square-tree-1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (tree-map-2 proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map-2 proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree-2 tree) (tree-map-1 square tree))

(square-tree-2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
