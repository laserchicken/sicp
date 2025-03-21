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
