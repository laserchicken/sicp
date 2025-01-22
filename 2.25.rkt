#lang sicp

(define x '(1 3 (5 7) 9))

(car (cdr (car (cdr (cdr x)))))

(define y '((7)))

(car (car y))

(define z '(1 (2 (3 (4 (5 (6 7)))))))

;(cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)) null)) null)) null))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))))

