#lang sicp

(define (deep-reverse tree)
  (define (deep-reverse-iter tree rev)
    (cond ((null? tree) rev)
          ((not (pair? (car tree)))
           (deep-reverse-iter (cdr tree) (cons (car tree) rev)))
          (else
           (deep-reverse-iter (cdr tree) (cons (deep-reverse (car tree)) rev)))))
  (deep-reverse-iter tree nil))

(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list 1 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 3 4))

(define (deep-rev tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (list tree))
        (else
         (cons (deep-rev (car tree)) (deep-rev (cdr tree))))))

(newline)
(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list 1 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 (list 3 4)))
(deep-reverse (list (list 0 1) 2 3 4))
