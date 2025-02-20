#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;(/ 1 (/ 2 (/ 3 1)))
;3/2

(fold-left / 1 (list 1 2 3))
;(iter 1 '(1 2 3)
;(iter (/ 1 1) '(2 3))
;(iter (/ 1 2) '(3))
;(iter (/ 1/2 3) '())
;1/6

(fold-right list nil (list 1 2 3))
;(list 1 (list 2 (list 3 nil)))
;'(1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
;(iter (list '() 1) '(2 3))
;(iter (list '(() 1) 2) '(3))
;(iter (list '((() 1) 2) 3) '())
;'(((() 1) 2) 3)
