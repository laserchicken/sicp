#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


;;we want the broadest interval possible using subsctraction operation
;;min would be substracting max value from min value
;;max would be substracting min value from max value
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y)) ;;min we can get from the x,y bounds combination
                 (- (upper-bound x) (lower-bound y)))) ;;max we can get from the x,y bounds combination
  

;; good explanation here: https://jots-jottings.blogspot.com/2011/09/sicp-exercise-28-interval-subtraction.html
