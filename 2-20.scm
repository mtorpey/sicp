(define (same-parity x . xs)
  (define (filter-by-parity parity xs)
    (if (null? xs)
        xs
        (let ((head (car xs))
              (tail (filter-by-parity parity (cdr xs))))
          (if (eq? (even? head) parity)
              (cons head tail)
              tail))))
  (cons x (filter-by-parity (even? x) xs)))

(display
 (same-parity 1 2 3 4 5 6 7))
;; (1 3 5 7)
(newline)

(display
 (same-parity 2 3 4 5 6 7))
;; (2 4 6)
(newline)
