#lang sicp

;; From textbook
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (square x) (* x x))

(define (fermat-test n a)
  (= (expmod a n n) a))

;; From textbook, modified
(define (fast-prime-exhaustive? n)
  (define (fast-prime-iter n start)
    (cond ((= start n) true)
          ((fermat-test n start) (fast-prime-iter n (+ start 1)))
          (else false)))
  (fast-prime-iter n 0))

;; Non-primes: all false
(display
 (map fast-prime-exhaustive?
      (list 6 100 65536)))

;; Primes: all true
(display
 (map fast-prime-exhaustive?
      (list 2 17 101)))

;; Carmichael numbers: all true since they fool the test
(display
 (map fast-prime-exhaustive?
      (list 561 1105 1729 2465 2821 6601)))
