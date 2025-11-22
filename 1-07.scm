#lang sicp

;; good-enough? has a fixed tolerance of 0.001, but we may want to find the
;; square root of very small numbers for which 0.001 is actually a big
;; proportion of their value. For example, if we call sqrt on a number as small
;; as 0.0001 we may get a result whose square differs from the input by a factor
;; of 10.

;; With floating-point arithmetic, large numbers are stored with low
;; precision. The error in the value returned by sqrt-iter may be larger than
;; 0.001, so we may end up with sqrt-iter returning a worse guess than the one
;; it was given. The recursion could go on for an unbounded number of calls, and
;; indeed we might never get a guess that satisfies good-enough?. Calling sqrt
;; on 10^63 might well cause this behaviour. For large numbers we probably don't
;; need precision as small as 0.001 anyway.

(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess last-guess)
  (< (/ (abs (- guess last-guess))
        guess)
     0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

(define (average x y)
  (/ (+ x y) 2))

(define (compose f g) (lambda (x) (f (g x))))
(display
 (map (compose sqrt square)
      (list 0.0001 10 2 99999999999)))
