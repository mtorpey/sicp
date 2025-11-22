#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

;; Simpler, with a recursive process
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

;; More complicated, with an iterative process
(define (repeated-it f n)
  ;; fi = f^i
  (define (iter i fi)
    (if (= i n)
        fi
        (iter (+ i 1) (compose fi f))))
  (iter 0 identity))

;; Testing
(define (square x) (* x x))
(display
 ((repeated square 2) 5))
(display
 ((repeated-it square 2) 5))
