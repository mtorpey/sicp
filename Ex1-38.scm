;;
;; Iterative version from last exercise
;;
(define (cont-frac-it n d k)
  (define (iter k inner)
    (if (= k 0)
        inner
        (iter (- k 1) (/ (n k) (+ (d k) inner)))))
  (iter k 0))

;;
;; Applying it to Euler's expansion for e-2
;;
(define (euler-n i) 1)
(define (euler-d i)
  (if (= (remainder i 3) 2)
      (* (/ (+ i 1) 3) 2)
      1))
(define (euler-e k)
  (+ 2.0 (cont-frac-it euler-n euler-d k)))

;;
;; Test
;;
(display (euler-e 10))
