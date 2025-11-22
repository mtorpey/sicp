#lang sicp

;; First we try a slightly different approach.
;; From §1.2.2 we know that φ is the ratio such that
;;   φ² = φ+1,
;; or equivalently,
;;   φ = φ²−1.
;; Hence it is a fixed point of the function
;;   y ↦ y²−1.
;; We find an approximation of it using the fixed-point procedure as follows.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)  ; Show progress for the sake of the exercise
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Note that applying fixed-point directly via
;;   (fixed-point (lambda (y)
;;                  (- (* y y) 1))
;;                1.0)
;; fails to converge, oscillating between 0 and −1. So we instead use average
;; damping, and search for a fixed point of the function
;;   y ↦ ½(y + y²−1).

(display
 (fixed-point (lambda (y)
                (/ (+ y (* y y) -1) 2))
              1.0))

;; This converges, but finds a negative solution at −0.62, whereas we know the
;; real φ is positive. The problem is the −1 in the formula, but we can avoid
;; this by dividing φ = φ²−1 through by φ to get 1 = φ − 1/φ, or
;;   φ = 1 + 1/φ,
;; which means that φ is a fixed point of the function
;;   y ↦ 1 + 1/y,
;; which we'll pursue as suggested in the exercise.

(display
 (fixed-point (lambda (y)
                (+ 1 (/ 1 y)))
              1.0))
