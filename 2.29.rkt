#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

(define (branch-weight b)
  (if (not (pair? (branch-structure b)))
      (branch-structure b)
      (mobile-weight (branch-structure b))))

(define (mobile-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define x (make-branch 1 1))
(define m1 (make-mobile x x))
(define m2 (make-mobile (make-branch 1 m1) x))
(define m (make-mobile (make-branch 1 m2) (make-branch 1 m2)))

(mobile-weight m)

(define m3 (make-mobile (make-branch 1 2) (make-branch 3 4))) 
(mobile-weight m3)

(define n
  (make-mobile
   (make-branch
    1
    (make-mobile
     (make-branch 1 2)
     (make-branch 3 4)))
   (make-branch
    2
    (make-mobile
     (make-branch 2 2)
     (make-branch 3 1))))) 

(mobile-weight n)
