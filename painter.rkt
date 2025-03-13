#lang racket
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
  (car frame))

(define (edge1-frame-2 frame)
  (car (cdr frame)))

(define (edge2-frame-2 frame)
  (cdr (cdr frame)))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s)))

(define (make-segment start-v end-v)
  (cons start-v end-v))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (vector-to-posn v)
  (make-posn (xcor-vect v) (ycor-vect v)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (line
        (vector-to-posn ((frame-coord-map frame)
         (start-segment segment)))
        (vector-to-posn ((frame-coord-map frame)
         (end-segment segment)))))
     segment-list)))

((segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                          (make-segment (make-vect 0 1) (make-vect 1 0))))
                    (make-frame (make-vect 100 100) (make-vect 0 300) (make-vect 300 0)))
