#lang sicp

(define (equal? l1 l2)
  (let ((is-l1-pair? (pair? l1))
        (is-l2-pair? (pair? l2)))
    (cond ((and is-l1-pair? is-l2-pair?)
           (and (equal? (car l1) (car l2))
                (equal? (cdr l1) (cdr l2))))
          ((or (and is-l1-pair? is-l2-pair?)
               (and (not is-l1-pair?) is-l2-pair?))
           #f)
          (else
           (eq? l1 l2)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '(this (is (a b)) list) '(this (is (a b)) list))
