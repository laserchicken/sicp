#lang sicp

(define (same-parity x . rest)
  (define (find-same-parity lst acc)
    (if (null? lst)
        acc
        (if (or (and (even? (car lst)) (even? x))
                (and (not (even? (car lst))) (not (even? x))))
            (find-same-parity (cdr lst) (cons (car lst) acc))
            (find-same-parity (cdr lst) acc))))
  (reverse (find-same-parity rest (list x))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
