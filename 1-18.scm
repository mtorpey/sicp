#lang sicp

;; Elemental operations
(define (double x) (* x 2))
(define (halve x) (/ x 2))

;; Logarithmic multiplication
(define (fast-mult-iter a b)
  (define (fmit a b value)
    (cond ((= b 0) value)
          ((even? b) (fmit (double a) (halve b) value))
          (else (fmit a (- b 1) (+ value a)))))
  (fmit a b 0))

;; Test
(display
 (list (* 5 7)
       (* 11 17)
       (* 100 101)))
