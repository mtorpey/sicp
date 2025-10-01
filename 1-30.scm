;; My solution
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; For testing
(define (cube x) (* x x x))
(define (identity x) x)
(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; Tests
(display (list (sum-cubes 1 10)
               (sum-integers 1 10)
               (* 8 (pi-sum 1 1000))))
