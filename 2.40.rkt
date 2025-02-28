#lang sicp

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) ;;this flattens list in flatmap
          (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval i n)
  (if (> i n)
      nil
      (cons i (enumerate-interval (+ i 1) n))))

(define (unique-pairs n)
  (flatmap (lambda (i)
         (map (lambda (j)
                (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else
         (filter predicate (cdr seq)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
