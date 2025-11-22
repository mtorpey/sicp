#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

;; double applies its argument twice
;; double double applies its argument 4 times
;; double (double double) applies its argument 4×4=16 times
;; (double (double double) inc) adds 16
;; Output should be 21
(display
 (((double (double double)) inc) 5))
