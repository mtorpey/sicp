#lang sicp

(define (reverse items)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items)
              (cons (car items) acc))))
  (iter items nil))

(display
 (reverse (list 1 4 9 16 25)))
