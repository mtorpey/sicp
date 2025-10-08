(define (last-pair items)
  (if (= (length items) 1)
      (car items)
      (last-pair (cdr items))))

(display
 (last-pair (list 23 72 149 34)))
