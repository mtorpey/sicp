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
(define (tan-cf x k)
  (define (n i) (if (= i 1) x (- (* x x))))
  (define (d i) (- (* 2 i) 1))
  (cont-frac-it n d k))

;;
;; Test
;; Should be approximately (0 0 1 1.7321 ±infty ±infty)
;;
(define pi 3.141592654)
(display (map (lambda (x) (tan-cf x 10))
              (list 0.0
                    pi
                    (/ pi 4)
                    (/ pi 3)
                    (/ pi 2)
                    (/ pi -2))))
