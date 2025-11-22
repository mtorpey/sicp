#lang sicp

;; Adapted from the book 
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)  ; Display progress
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Without damping: x ↦ log(1000) / log(x).
;; Using 2 as start guess.
(display
 (fixed-point (lambda (x) (/ (log 1000) (log x)))
              2.0))
(newline)

;; With damping: x ↦ ½(x + log(1000)/log(x)).
(display
 (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
              2.0))
(newline)

;; Without damping takes 34 steps; with damping takes only 9.
