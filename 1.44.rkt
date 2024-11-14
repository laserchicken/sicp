#lang sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-iter f n)
  (lambda (x)
    (define (repeated f n acc)
      (if (= n 1)
          (acc x)
          (repeated f (- n 1) (compose f acc))))
    (repeated f n f)))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

((smooth square) 0)
((smooth square) 1)
((smooth square) 2)

(define (n-smooth f n)
  ((repeated-iter smooth n) f))

((n-smooth square 3) 0)
((n-smooth square 3) 1)
((n-smooth square 3) 2)
