#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 '(1 2 3 4 5))
(accumulate max 0 '(0 3 2 8 4))

(define (map-1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map-1 sqrt '(1 4 9 16))

(define (append-1 seq1 seq2)
  (accumulate cons seq2 seq1))

(append-1 '(1 2 3) '(4 5 6))

(define (length-1 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length-1 '())
(length-1 '(1))
(length-1 '(1 2 3 4 5))
