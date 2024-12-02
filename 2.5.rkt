#lang sicp

(define (even? n) (= (remainder n 2) 0))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (rid-of-facator x factor)
  (if (not (= (remainder x factor) 0))
      x
      (rid-of-facator (/ x factor) factor)))

(define (car x)
  (log (rid-of-facator x 3) 2))

(define (cdr x)
  (log (rid-of-facator x 2) 3))

(define a (cons 0 0))

(car a)
(cdr a)


(define b (cons 0 1))

(car b)
(cdr b)


(define c (cons 1 0))

(car c)
(cdr c)

(define d (cons 1 1))

(car d)
(cdr d)

(define e (cons 2 1))

(car e)
(cdr e)

(define f (cons 1 2))

(car f)
(cdr f)

(define g (cons 2 3))

(car g)
(cdr g)

(define h (cons 3 2))

(car h)
(cdr h)

(define i (cons 3 4))

(car i)
(cdr i)

(define j (cons 4 3))

(car j)
(cdr j)
