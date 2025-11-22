#lang sicp

;; Elemental operations
(define (double x) (* x 2))
(define (halve x) (/ x 2))

;; Logarithmic multiplication
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

;; Test
(display
 (list (* 5 7)
       (* 100 101)))
