#lang sicp

(define (even? n) (= (remainder n 2) 0))

(define (cont-frac n d x k)
  (define (cont-frac-iter n d x i acc)
    (if (= i 0)
        acc
        (if (even? i)
            (cont-frac-iter n d x (- i 1) (- (* (/ 1 acc) (n x)) (d i)))
            (cont-frac-iter n d x (- i 1) (+ (* (/ 1 acc) (n x)) (d i))))))
  (/ x (cont-frac-iter n d x k (d k))))

(define (n x)
  (* x x))

(define (d i)
  (- (* 2 i) 1))

(define (tan-cf x k)
  (cont-frac n d x k))

(tan-cf 0.2094 15)
(tan-cf 0.7854 15)
