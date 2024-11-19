#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-iter f n)
  (lambda (x)
    (define (repeated f n acc)
      (if (= n 1)
          (acc x)
          (repeated f (- n 1) (compose f acc))))
    (repeated f n f)))

(define (average x y) (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (square x) (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (exp b n)
  (define (exp-iter b a n)
    (cond ((= n 0) a)
          ((even? n) (exp-iter (square b) a (/ n 2)))
          (else (exp-iter b (* a b) (- n 1)))))
  (exp-iter b 1 n))

(define (root x n)
  (fixed-point ((repeated-iter average-damp (- n 1)) (lambda (y) (/ x (exp y (- n 1)))))
               1.0))

(root 9 2)
(root 27 3)
(root 16 4)
(root 81 4)
(root 32 5)
