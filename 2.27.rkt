#lang sicp

(define (deep-reverse list)
  (define (deep-reverse-iter list rev)
    (cond ((null? list) rev)
          ((not (pair? (car list)))
           (deep-reverse-iter (cdr list) (cons (car list) rev)))
          (else
           (deep-reverse-iter (cdr list) (cons (deep-reverse (car list)) rev)))))
  (deep-reverse-iter list nil))

(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list 1 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 3 4))

(define (deep-rev list)
  (cond ((null? list) nil)
        ((not (pair? (car list)))
         (cons (car list) (deep-rev (cdr list))))
        (else
         (cons (deep-rev (car list)) (deep-rev (cdr list))))))

(newline)
(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list 1 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 3 4))
