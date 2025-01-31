#lang sicp

(define (make-mobile left right) (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m)) ;this changed

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b)) ;this changed

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

(define m4 (make-mobile (make-branch 1 m1) (make-branch 1 m1)))

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

(define (torque b)
  (* (branch-weight b)
     (branch-length b)))

(define (is-balanced? m)
  (if (not (pair? m))
      true
      (and (= (torque (left-branch m))
              (torque (right-branch m)))
           (and (is-balanced? (branch-structure (left-branch m)))
                (is-balanced? (branch-structure (right-branch m)))))))

(define m_unbalanced (make-mobile (make-branch 1 2)
                                  (make-branch 1 (make-mobile (make-branch 0.5 3)
                                                              (make-branch 2 4)))))

(define m_balanced (make-mobile (make-branch 1 2)
                                (make-branch 1 (make-mobile (make-branch 1 1)
                                                            (make-branch 1 1)))))

(is-balanced? n)
(is-balanced? m)
(is-balanced? m_unbalanced)

(is-balanced? m1)
(is-balanced? m4)
(is-balanced? m_balanced)
