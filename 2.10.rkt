#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (have-different-sign x y)
  (< (* x y) 0))

(define (div-interval x y)
  (let ((y-lower-bound (lower-bound y))
        (y-upper-bound (upper-bound y)))
    (if (have-different-sign y-lower-bound y-upper-bound)
        (error "Interval spans 0")
        (mul-interval
         x
         (make-interval (/ 1.0 (upper-bound y))
                        (/ 1.0 (lower-bound y)))))))

(define a1 (make-interval 1 2))
(define b1 (make-interval -2 1))

(div-interval a1 b1)
