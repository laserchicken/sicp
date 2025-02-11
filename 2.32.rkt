#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append '(1 2 3) '(4 5 6))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x)
                       (append (list (car s)) x))
                     rest)))))

(subsets '(3))
(subsets '(2 3))
(subsets '(1 2 3))
