#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))
(define (square x) (* x x))

;; Should return (6 + 1)² = 49
(display
 ((compose square inc) 6))
