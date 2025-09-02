;; Pascal's triangle: row n, the kth element, both indexed from 0
(define (binomial n k)
  (cond ((or (< n 0) (< k 0) (> k n)) 0)
        ((or (= k 0) (= k n)) 1)
        (else (+ (binomial (- n 1) (- k 1))
                 (binomial (- n 1) k)))))

(display (binomial 4 0))
(display (binomial 4 1))
(display (binomial 4 2))
(display (binomial 4 3))
(display (binomial 4 4))
(display (binomial 4 5))
