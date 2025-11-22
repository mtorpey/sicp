#lang sicp

;; Returns 0 if it finds a nontrivial square root of 1 mod m,
;; or otherwise (base ^ exp) mod m.
(define (expmod base exp m)
  (define (!= a b) (not (= a b)))

  ;; Squares x, mods it by m, then checks for nontrivial sqrts of 1.
  (define (square-mod-filter x)
    (define s
      (remainder (square x) m))
    (if (and (!= x 1) (!= x (- m 1)) (= s 1))
        0 ; should be 0 (but causes false negatives)
        s))

  ;; Main part
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-mod-filter (expmod base (/ exp 2) m)))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (square x) (* x x))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-mr? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime-mr? n 10))

;; Non-primes: all false
(display
 (map prime?
      (list 4 6 51 100 28000 65536)))

;; Primes: all true
(display
 (map prime?
      (list 2 3 5 7 11 13 17 19 47 101 14549)))

;; Carmichael numbers: all false since they cannot fool this test
(display
 (map prime?
      (list 561 1105 1729 2465 2821 6601)))
