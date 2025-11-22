#lang sicp

;; From book
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

;; My corresponding cdr
(define (cdr z)
  (z (lambda (p q) q)))

;; Testing
(define pair1 (cons 10 20))
(display (car pair1))
(newline)
;; Rewrites as follows:
;;   (car pair1)
;;   (pair1 (lambda (p q) p))
;;   ((lambda (p q) p) 10 20)
;;   10

(display (cdr pair1))
(newline)
;; Rewrites as follows:
;;   (cdr pair1)
;;   (pair1 (lambda (p q) q))
;;   ((lambda (p q) q) 10 20)
;;   20
