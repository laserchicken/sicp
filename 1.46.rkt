#lang sicp

(define (iterative-improve is-good-enough improve)
  (define (helper guess)
    (if (is-good-enough guess)
        guess
        (helper (improve guess))))
  helper)

;;;;;;;sqrt

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess))))
   1.0))

(new-sqrt 9)
(new-sqrt 4)
(new-sqrt 25)


;;;;;;fixed-point
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(define (new-fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
                      (lambda (guess) (f guess)))
   first-guess))

(new-fixed-point cos 1.0)
