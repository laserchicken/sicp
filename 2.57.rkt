#lang sicp

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else
         (filter predicate (cdr seq)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (variable? x) (symbol? x))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-sub a1 a2)
  (cond ((=number? a1 0) (- a2))
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (- a1 a2))
        (else (list '- a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((rest-list (cddr s)))
    (if (null? (cdr rest-list))
        (car rest-list)
        (cons '+ rest-list))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((rest-list (cddr p)))
    (if (null? (cdr rest-list))
        (car rest-list)
        (cons '* rest-list))))

(define (exp? x)
  (and (pair? x) (eq? (car x) '^)))

(define (make-exp x y) 
  (cond ((=number? x 0) 0)
        ((=number? y 0) 1)
        ((=number? x 1) 1)
        ((=number? y 1) x)
        ((and (number? x) (number? y))
         (expt x y))
        (else (list '^ x y))))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exp? exp)
         (make-product (exponent exp)
                       (make-product (make-exp (base exp) (make-sub (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(+ 1 2 3 x) 'x)
(deriv '(+ x y z 1) 'x)
(deriv '(+ x y 1) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* (* x y) (+ x 3) (+ y 3)) 'x)
(deriv '(^ (* 2 x) 3) 'x)
