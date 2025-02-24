#lang sicp

(define (reverse-1 items)
  (define (reverse-iter result rest)
    (if (null? rest)
        result
        (reverse-iter (cons (car rest) result) (cdr rest))))
  (reverse-iter '() items))

(define (reverse-2 ls)
  (if (null? ls) 
	'()
	(append (reverse-2 (cdr ls)) (list (car ls)))))

(reverse-1 (list 1 4 9 16 25))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (reverse-3 sequence)
  (fold-right (lambda (x y) (append y (list x) )) nil sequence))

(reverse-3 (list 1 4 9 16 25))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-4 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse-4 (list 1 4 9 16 25))
