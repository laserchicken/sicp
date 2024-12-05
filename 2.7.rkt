#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

(define i (make-interval 2 5))
(lower-bound i)
(upper-bound i)
