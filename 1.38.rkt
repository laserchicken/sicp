#lang sicp

(define (d i)
  (cond ((= i 1) 1)
        ((= i 2) 2)
        ((and (= (d (- i 2)) 1) (= (d (- i 1)) 1)) (+ (d (- i 3)) 2)) ;;last two 1, 1 - return 2*k
        ((and (= (d (- i 2)) 1)) (not (= (d (- i 1)) 1)) 1) ;;last two 1, 2*k - return 1
        ((and (not (= (d (- i 2)) 1)) (= (d (- i 1)) 1)) 1))) ;;last two 2*k, 1 - return 1

(d 1)
(d 2)
(d 3)
(d 4)
(d 5)
(d 6)
(d 7)
(d 8)
(d 9)
(d 10)
(d 11)


(define (cont-frac n d k)
  (define (cont-frac-iter n d i acc)
    (if (= i 1)
        acc
        (cont-frac-iter n d (- i 1) (+ (* (/ 1 acc) (n i)) (d (- i 1))))))
  (/ (n 1) (cont-frac-iter n d k (d k))))

(define (calc-e k)
  (+ 2.0 
     (cont-frac (lambda (i) 1.0)
                d
                k)))

(calc-e 10)
