#lang sicp

(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq) (accumulate proc init (cdr seq)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1))
                       t)))

(display (count-leaves (list 1 (list 2 (list 3 4) 5) (list 6 7))))
