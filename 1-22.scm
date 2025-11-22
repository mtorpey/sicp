#lang sicp

;; From textbook
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

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

;; Output looks like this:
;;   1000000000039 *** 18
;;   1000000000061 *** 23
;;   1000000000063 *** 22
;;   10000000000037 *** 60
;;   10000000000051 *** 61
;;   10000000000099 *** 54
;;   100000000000031 *** 177
;;   100000000000067 *** 175
;;   100000000000097 *** 168
;;   1000000000000037 *** 918
;;   1000000000000091 *** 667
;;   1000000000000159 *** 531

;; The averages are 21, 58, 173, 705.
;; So multiplying n by 10 increases time by 2.8 or 3.0 or 4.1,
;; where sqrt(10) is 3.16, which isn't far off.
