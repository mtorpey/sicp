#lang sicp

;; Rectangles described by two corners
(define (make-rect1 p1 p2)
  (cons p1 p2))
(define (width-rect1 r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (height-rect1 r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

;; Rectangles described by one corner and width and height
(define (make-rect2 p1 width height)
  (cons p1 (cons width height)))
(define (width-rect2 r)
  (car (cdr r)))
(define (height-rect2 r)
  (cdr (cdr r)))

;; Procedures that are implementation-independent
;;
;; These functions take width and height procedures and returns a function that
;; gives the required value for a rectangle with that representation.
(define (perimeter width height)
  (lambda (r) (+ (* 2 (width r)) (* 2 (height r)))))
(define (area width height)
  (lambda (r) (* (width r) (height r))))

;; Points
(define make-point cons)
(define x-point car)
(define y-point cdr)

;; Tests: representation 1
(define perimeter-rect1 (perimeter width-rect1 height-rect1))
(define area-rect1 (area width-rect1 height-rect1))
(define r1 (make-rect1 (make-point 2 3) (make-point 4 4)))
(display (perimeter-rect1 r1))
(newline)
(display (area-rect1 r1))
(newline)

;; Tests: representation 2
(define perimeter-rect2 (perimeter width-rect2 height-rect2))
(define area-rect2 (area width-rect2 height-rect2))
(define r2 (make-rect2 (make-point 2 3) 2 1))
(display (perimeter-rect2 r2))
(newline)
(display (area-rect2 r2))
(newline)
