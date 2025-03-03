#lang sicp

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

(define (triples-of-distinct-integers n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
                        (map (lambda (k) 
                                   (list i j k))
                             (enumerate-interval (+ j 1) n)))
                      (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 (- n 2))))


(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else
         (filter predicate (cdr seq)))))

(triples-of-distinct-integers 5)

(define (sums-to? s triple)
  (= (accumulate + 0 triple) s))

(define (find-triples n s)
  (filter (lambda (triple) (sums-to? s triple)) (triples-of-distinct-integers n)))

(find-triples 5 8)
(find-triples 5 12)
(find-triples 6 12)


