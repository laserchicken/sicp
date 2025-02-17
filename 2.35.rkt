#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)
(count-leaves (list x x))

(define (count-leaves-acc t)
;  (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))
  (accumulate + 0 (map (lambda (tree)
                         (if (not (pair? tree))
                             1
                             (count-leaves-acc tree)))
                       t)))

(count-leaves-acc x)
(count-leaves-acc (list x x))
