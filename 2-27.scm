(define (deep-reverse items)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items)
              (cons (if (pair? (car items))
                        (deep-reverse (car items))
                        (car items))
                    acc))))
  (iter items null))

(define x (list (list 1 2) (list 3 4)))
(display (deep-reverse x))
