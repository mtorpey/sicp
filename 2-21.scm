(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list (cdr items)))))
(display (square-list (list 1 2 3 4)))

(define (square-list items)
  (map square items))
(display (square-list (list 1 2 3 4)))
