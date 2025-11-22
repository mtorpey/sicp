#lang sicp

;; My solutions: same as previous sum procedures but with (*,1) instead of (+,0)
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; For testing
(define (identity x) x)
(define (inc n) (+ n 1))

(define (factorial-rec n) (product-rec identity 1 inc n))
(define (factorial-it  n) (product-it  identity 1 inc n))

(define (pi-term n)
  (/ (* n (+ n 2))
     (* (+ n 1) (+ n 1))))
(define (pi-next n)
  (+ n 2))

(define (pi-prod-rec n) (product-rec pi-term 2.0 pi-next n))
(define (pi-prod-it  n) (product-it  pi-term 2.0 pi-next n))

;; Tests
(display (list (factorial-rec 10)
               (factorial-it 10)
               (* 4 (pi-prod-rec 1000))
               (* 4 (pi-prod-it  1000))))

;; Results (3628800 3628800 3.1431607055322752 3.1431607055322712)
