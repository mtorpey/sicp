#lang sicp

;; Helper
(define (compose f g) (lambda (x) (f (g x))))

;; Iterative version for performance
(define (repeated f n)
  (define (iter i fi)
    (if (= i n)
        fi
        (iter (+ i 1) (compose fi f))))
  (iter 0 identity))

;; From book
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (let ((distance (abs (- v1 v2))))
      (display ".") ; lightweight indicator
      (< distance tolerance)))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

;;
;; Experiments to see what converges
;;
(define (nth-root-with-damps x n damps)
  (fixed-point ((repeated average-damp damps)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; 2nd and 3rd roots require one average-damp
(display (nth-root-with-damps 9 2 1))
(newline)
(display (nth-root-with-damps 27 3 1))
(newline)

;; 4th root converges extremely slowly with one damp, but quickly with 2
(display (nth-root-with-damps 81 4 2))
(newline)

;; 5th to 7th roots fail to converge with one damp, but go quickly with 2
(display (nth-root-with-damps 243 5 2))
(newline)
(display (nth-root-with-damps 729 6 2))
(newline)
(display (nth-root-with-damps 2187 7 2))
(newline)

;; 8th root converges extremely slowly with 2 damps, but quickly with 3
(display (nth-root-with-damps 6561 8 3))
(newline)

;; 9th to 15th roots require 3 damps
(display (nth-root-with-damps 19683 9 3))
(newline)
(display (nth-root-with-damps 59049 10 3))
(newline)
(display (nth-root-with-damps (expt 3 11) 11 3))
(newline)
(display (nth-root-with-damps (expt 3 12) 12 3))
(newline)
(display (nth-root-with-damps (expt 3 13) 13 3))
(newline)
(display (nth-root-with-damps (expt 3 14) 14 3))
(newline)
(display (nth-root-with-damps (expt 3 15) 15 3))
(newline)

;; 16th root converges extremely slowly with 3 damps, but quickly with 4
(display (nth-root-with-damps (expt 3 16) 16 4))
(newline)

;; Pattern: n damps are enough for anything up to the 2^(n+1)th root.
;; Equivalently: computing the nth root requires ⌊log₂n⌋ damps.
;; If n is a power of 2, it may be that one less is enough, but it's unreasonably slow.
(define (nth-root x n)
  (nth-root-with-damps x n (floor (log n 2))))

;; Test using 5 as the base
(display (map (lambda (n) (nth-root (expt 5 n) n))
              (list 1 2 3 4 5 6 7 8 9 10 30 50)))
