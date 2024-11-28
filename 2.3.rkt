#lang sicp

(define (square x) (* x x))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (length-segment s)
  (sqrt (+ (square (abs (- (x-point (start-segment s)) (x-point (end-segment s)))))
           (square (abs (- (y-point (start-segment s)) (y-point (end-segment s))))))))

(define (print-segment s)
  (print-point (start-segment s))
  (print-point (end-segment s)))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;rect constructor takes a and b segments
;;they should start in the same origin
;;and be perpendicular

(define (make-ab-rect a b)
  (cons a b))

(define (a-ab-rect rect)
  (car rect))

(define (b-ab-rect rect)
  (cdr rect))

(define (perimeter-ab-rect rect)
  (+ (* 2 (length-segment (a-ab-rect rect)))
     (* 2 (length-segment (b-ab-rect rect)))))

(define (area-ab-rect rect)
  (* (length-segment (a-ab-rect rect)) (length-segment (b-ab-rect rect))))

(define a1 (make-segment (make-point 1 1) (make-point 5 5)))
(define b1 (make-segment (make-point 1 1) (make-point 2 0)))
(define rect1 (make-ab-rect a1 b1))

(perimeter-ab-rect rect1)
(area-ab-rect rect1)

;;rect constructor takes diagonals d1 and d2
;;they should intersect in the middle, non-zero angle between them

(define (make-diag-rect d1 d2)
  (cons d1 d2))

(define (d1-rect rect)
  (car rect))

(define (d2-rect rect)
  (cdr rect))

(define (a-diag-rect rect)
  (make-segment (start-segment (d1-rect rect))
                (end-segment (d2-rect rect))))

(define (b-diag-rect rect)
  (make-segment (start-segment (d1-rect rect))
                (start-segment (d2-rect rect))))

(define (perimeter-diag-rect rect)
  (+ (* 2 (length-segment (a-diag-rect rect)))
     (* 2 (length-segment (b-diag-rect rect)))))

(define (area-diag-rect rect)
  (* (length-segment (a-diag-rect rect)) (length-segment (b-diag-rect rect))))

(define d1 (make-segment (make-point 1 1) (make-point 6 4)))
(define d2 (make-segment (make-point 2 0) (make-point 5 5)))
(define rect2 (make-diag-rect d1 d2))

(perimeter-diag-rect rect2)
(area-diag-rect rect2)
