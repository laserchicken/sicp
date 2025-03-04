#lang sicp

;'((1 3 5 ...)
;  (2 4 1 ...)

;'(((1 0 0 0 0 0 0 0) (0 0 1 0 0 0 0 0) (0 0 0 0 1 0 0 0) ...
;  ((0 1 0 0 0 0 0 0) (0 0 0 1 0 0 0 0) (1 0 0 0 0 0 0 0) ...

(define (enumerate-interval i n)
  (if (> i n)
      nil
      (cons i (enumerate-interval (+ i 1) n))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else
         (filter predicate (cdr seq)))))

(define empty-board '())

(define (safe? k positions)
  (let ((reverse-positions (reverse positions)))
    (define (safe-reverse-iter i last-position seq)
      (cond ((= i k) true)
            ((= (car seq) last-position) false)
            ((= (car seq) (- last-position i)) false)
            ((= (car seq) (+ last-position i)) false)
            (else
             (safe-reverse-iter (+ i 1) last-position (cdr seq)))))
    (safe-reverse-iter 1 (car reverse-positions) (cdr reverse-positions))))

(safe? 4 '(3 7 2 8 5 1 4 6))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (cons new-row nil)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)
(queens 5)
(queens 8)
