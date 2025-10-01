;; Special random procedure for large numbers
(define (randlarge n)
  (define max 4294967087)
  (remainder (+ (* max (random max))
                (random max))
             n))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (randlarge (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; From exercise, adapted for nicer printing
(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 50)
      (report-prime n (- (current-milliseconds) start-time))
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

;; Computers are much too fast for this. This test takes no appreciable time
;; with numbers even at 10^50 (I didn't bother adapting randlarge to work for
;; numbers this big). It's a fast test!
(search-for-primes (+ (expt 10 50) 1) 2 3)

;; To answer the question, I would expect primes near 1,000,000 to take twice as
;; long as 1,000 for a logarithmic algorithm.
