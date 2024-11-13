#lang sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (define (helper f n)
      (if (= n 1)
          (f x)
          (f (helper f (- n 1)))))
    (helper f n)))


(define (repeated-iter f n)
  (lambda (x)
    (define (repeated f n acc)
      (if (= n 1)
          (acc x)
          (repeated f (- n 1) (compose f acc))))
    (repeated f n f)))

((repeated-iter square 2) 5)
