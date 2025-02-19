#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(1 2 3))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose '((1 2 3) (4 5 6) (7 8 9)))
;((1 4 7) (2 5 8) (3 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9))
                 '((1 2 3) (4 5 6) (7 8 9)))

