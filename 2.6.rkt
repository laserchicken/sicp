#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;---- 0
;(lambda (f) (lambda (x) x))
;---- 1
;(add-1 zero)
;(lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x) f) x))))
;(lambda (f) (lambda (x) (f x)))
;---- 2
;(add-1 (lambda (f) (lambda (x) (f x)))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;(lambda (f) (lambda (x) (f (f x))))
;
;addition is applying function f one more time to the argument
