#lang sicp

;; From book
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; My solutions
(define (map p sequence)
  (accumulate (lambda (item ptail) (cons (p item) ptail))
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (item count) (+ count 1))
              0
              sequence))

;; Tests
(define (square x) (* x x))
(display (map square (list 2 4 6 8)))
(display (append (list 0 7 8 0 0) (list 4 5 0 2 3 9)))
(display (length (list 1 3 4 5 2)))
