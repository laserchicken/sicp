#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-rec n d k i)
    (if (= i k)
        (d i)
        (+ (d i) (/ (n (+ i 1)) (cont-frac-rec n d k (+ i 1))))))
  (/ (n 1) (cont-frac-rec n d k 1)))

(define (calc-golden-ratio k)
  (/ 1.0 
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))

(calc-golden-ratio 12)

(define (cont-frac-i n d k)
  (define (cont-frac-iter n d i acc)
    (if (= i 1)
        acc
        (cont-frac-iter n d (- i 1) (+ (* (/ 1 acc) (n i)) (d (- i 1))))))
  (/ (n 1) (cont-frac-iter n d k (d k))))

(define (calc-golden-ratio-i k)
  (/ 1.0 
     (cont-frac-i (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))

(calc-golden-ratio-i 12)
