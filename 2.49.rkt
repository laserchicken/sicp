#lang sicp

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (make-segment start-v end-v)
  (cons start-v end-v))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define outline
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1))
                           (make-segment (make-vect 1 1) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define X
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond
  (segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                           (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

(define wave
  (segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                           (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))
