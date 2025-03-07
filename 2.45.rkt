#lang sicp

(define (split o1 o2)
  (define (split-helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-helper painter (- n 1))))
          (o1 painter (o2 smaller smaller)))))
  (lambda (painter n) (split-helper painter n)))

(define right-split (split beside below))
(define up-split (split below beside))
