;;
;; Recursive version
;;
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

;; Derived procedures
(define (sum-rec term a next b)
  (accumulate-rec + 0 term a next b))
(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))

;; Testing
(define (identity x) x)
(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (sum-cubes-rec a b) (sum-rec cube a inc b))
(define (factorial-rec n) (product-rec identity 1 inc n))
(display (list (sum-cubes-rec 1 10)
               (factorial-rec 10)))

;;
;; Iterative version
;;
(define (accumulate-it combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Derived procedures
(define (sum-it term a next b)
  (accumulate-it + 0 term a next b))
(define (product-it term a next b)
  (accumulate-it * 1 term a next b))

;; Testing
(define (sum-cubes-it a b) (sum-it cube a inc b))
(define (factorial-it n) (product-it identity 1 inc n))
(display (list (sum-cubes-it 1 10)
               (factorial-it 10)))
