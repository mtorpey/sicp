#lang sicp

(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq) (accumulate proc init (cdr seq)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(display (horner-eval 2 (list 1 3 0 5 0 1)))
