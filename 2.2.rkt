#lang sicp

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define s1 (make-segment (make-point 1 1) (make-point 5 5)))

(print-point (midpoint-segment s1))

(define s2 (make-segment (make-point 1 1) (make-point 4 4)))

(print-point (midpoint-segment s2))

(define s3 (make-segment (make-point 1 5) (make-point 5 1)))

(print-point (midpoint-segment s3))

(define s4 (make-segment (make-point 1 4) (make-point 4 1)))

(print-point (midpoint-segment s4))

(define s5 (make-segment (make-point 1 3) (make-point 5 3)))

(print-point (midpoint-segment s5))

(define s6 (make-segment (make-point 3 1) (make-point 3 5)))

(print-point (midpoint-segment s6))
