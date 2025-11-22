#lang sicp

(define (my-for-each f items)
  (if (null? items)
      true
      ((lambda (item items)
         (f item)
         (my-for-each f items))
       (car items)
       (cdr items))))

(display
 (my-for-each (lambda (x)
                (newline)
                (display x))
              (list 57 321 88)))
