#lang sicp

(define (fringe tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(display (fringe '(1 3 (5 7) 9)))
(display (fringe '((7))))
(display (fringe '(1 (2 (3 (4 (5 (6 7))))))))
