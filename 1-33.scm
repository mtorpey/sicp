;; Iterative version only
(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter? a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

;; From Section 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; From Exercise 1.28 
(define (expmod base exp m)
  (define (!= a b) (not (= a b)))
  (define (square-mod-filter x)
    (define s
      (remainder (square x) m))
    (if (and (!= x 1) (!= x (- m 1)) (= s 1))
        0
        s))
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
  (if (< n 2) false (fast-prime-mr? n 10)))

;; Helpers
(define (identity n) n)
(define (inc n) (+ n 1))
(define (square n) (* n n))

;; Derived procedures
(define (sum-squares-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-coprimes-under n)
  (define (coprime-n? k) (= 1 (gcd n k)))
  (filtered-accumulate * 1 identity 1 inc n coprime-n?))

;; Testing
(display (list (sum-squares-primes 1 10)
               (product-coprimes-under 20)))
