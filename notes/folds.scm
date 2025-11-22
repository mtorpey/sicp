;;
;; fold-right (also called accumulate)
;;
;; fold-right(·, id, x)  =  x₁·(x₂·(…(xₙ·id)…))
;;

;; Recursive
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

;; Iterative (would need unnatural operations last and all-but-last)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (last rest) result)
              (all-but-last rest))))
  (iter initial sequence))


;;
;; fold-left
;;
;; fold-left(·, id, x)  =  (…((id·x₁)·x₂)…)·xₙ
;;
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
