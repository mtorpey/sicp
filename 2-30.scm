;; Primitive
(define (square-tree1 t)
  (cond ((null? t) null)
        ((pair? t) (cons (square-tree1 (car t)) (square-tree1 (cdr t))))
        (else (* t t))))

;; Using map abstraction
(define (square-tree2 t)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (* sub-tree sub-tree)))
       t))

;; Tests
(define mytree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(display (square-tree1 mytree))
(display (square-tree2 mytree))
