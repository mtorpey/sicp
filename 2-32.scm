#lang sicp

;; Solution with a lambda
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (xs)
                            (cons (car s) xs))
                          rest)))))

;; Solution with currying
(define (c proc arg) (lambda (arg2) (proc arg arg2)))
(define (subsets2 s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (c cons (car s)) rest)))))

(display (subsets (list 1 2 3)))
(newline)
(display (subsets2 (list 1 2 3)))
