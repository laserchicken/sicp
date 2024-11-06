#lang sicp

(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1)
((double inc) 3)
(((double (double double)) inc) 5)

;(((double (lambda (x) (double (double x)))) inc) 5)
;(((lambda (x) (double (double (double (double x))))) inc) 5)
;((double (double (double (double inc)))) 5)
;((double (double (double (lambda (x) (inc (inc x)))))) 5)
;((double (double (lambda (x) (inc (inc (inc (inc x))))))) 5)
;((double (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)
;((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))))))))) 5)
;(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
;21
