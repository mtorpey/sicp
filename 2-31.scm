;; Primitive
(define (tree-map proc tree)
  (cond ((null? tree) null)
        ((pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree))))
        (else (proc tree))))

;; Using map abstraction
(define (tree-map2 proc tree)
  (map (lambda (sub)
         (if (pair? sub)
             (tree-map2 proc sub)
             (proc sub)))
       tree))

;; Tests
(define mytree
  (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (square x) (* x x))

(display (tree-map square mytree))
(display (tree-map2 square mytree))
