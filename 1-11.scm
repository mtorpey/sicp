#lang sicp

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive(- n 1))
         (* 2 (f-recursive(- n 2)))
         (* 3 (f-recursive(- n 3))))))

(define (f-iterative n)
  ;; For use when n >= 3
  (define (fit count last llast lllast)
    (define next
      (+ last (* 2 llast) (* 3 lllast)))
    (if (= count 0)
        next
        (fit (- count 1) next last llast)))
  ;; Main call
  (if (< n 3)
      n
      (fit (- n 3) 2 1 0)))

(display (map f-recursive (list 0 1 2 3 10)))
(display (map f-iterative (list 0 1 2 3 10 50)))
