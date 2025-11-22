#lang sicp

;; From textbook
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next m)
    (if (= m 2) 3 (+ m 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square n) (* n n))

;; From exercise, adapted for nicer printing
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)

;; My code
(define (search-for-primes start step count)
  (cond ((= count 0) true)
        ((timed-prime-test start) (search-for-primes (+ start step)
                                                     step
                                                     (- count 1)))
        (else (search-for-primes (+ start step) step count))))

;; Computers have got a lot faster since this book was written.
;; I've started at 10^12
(search-for-primes (+ (expt 10 12) 1) 2 3)
(search-for-primes (+ (expt 10 13) 1) 2 3)
(search-for-primes (+ (expt 10 14) 1) 2 3)
(search-for-primes (+ (expt 10 15) 1) 2 3)

;; Output of original
;;   1000000000039 *** 18
;;   1000000000061 *** 24
;;   1000000000063 *** 25
;;   10000000000037 *** 70
;;   10000000000051 *** 66
;;   10000000000099 *** 53
;;   100000000000031 *** 182
;;   100000000000067 *** 175
;;   100000000000097 *** 174
;;   1000000000000037 *** 549
;;   1000000000000091 *** 561
;;   1000000000000159 *** 563

;; Output of new version
;;   1000000000039 *** 9
;;   1000000000061 *** 15
;;   1000000000063 *** 16
;;   10000000000037 *** 38
;;   10000000000051 *** 39
;;   10000000000099 *** 27
;;   100000000000031 *** 95
;;   100000000000067 *** 87
;;   100000000000097 *** 87
;;   1000000000000037 *** 283
;;   1000000000000091 *** 281
;;   1000000000000159 *** 275

;; Findings: the time is very close to halved, which is what we'd expect.
