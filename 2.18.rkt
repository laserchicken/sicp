#lang sicp

(define (reverse list)
  (define (reverse-helper list rev)
    (if (null? list)
        rev
        (reverse-helper (cdr list) (cons (car list) rev))))
  (reverse-helper list nil))

(reverse (list 1 4 9 16 25))
