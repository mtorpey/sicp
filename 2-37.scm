#lang sicp

;; Helpers
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;; Definitions
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))


;; Tests
(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((0 1 3) (2 2 0) (1 4 3) (3 6 1)))
(define v '(1 0 1 2))
(define w '(2 3 4 5))

(display (dot-product v w))
(display (matrix-*-vector m v))
(display (transpose m))
(display (matrix-*-matrix m n))
