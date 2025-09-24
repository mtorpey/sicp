;;
;; Recursive version (works from outside in)
;;
(define (cont-frac-rec n d k)
  (define (cont-frac-starting-at i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-starting-at (+ i 1))))))
  (cont-frac-starting-at 1))

;; Testing
(display (map (lambda (k) (cont-frac-rec (lambda (n) 1.0)
                                         (lambda (d) 1.0)
                                         k))
              (list 1 2 3 4 5 6 7 8 9 10)))
;; k=10 is the first value to give 0.6180 (correct to 4dp)

;;
;; Iterative version (works from inside out)
;;
(define (cont-frac-it n d k)
  ; inner is the total fraction inside level k
  (define (iter k inner)
    (if (= k 0)
        inner
        (iter (- k 1) (/ (n k) (+ (d k) inner)))))
  (iter k 0))

;; Testing
(display (map (lambda (k) (cont-frac-it (lambda (n) 1.0)
                                        (lambda (d) 1.0)
                                        k))
              (list 1 2 3 4 5 6 7 8 9 10)))
