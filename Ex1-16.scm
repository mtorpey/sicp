(define (fast-expt-iter b n)
  (define (eit b n value)
    (cond ((= n 0) value)
          ((even? n) (eit (* b b) (/ n 2) value))
          (else (eit b (- n 1) (* value b)))))
  (define (even? n)
    (= (remainder n 2) 0))
  (eit b n 1))

;; Example expansion:
;; fast-expt-iter 2 11
;; eit 2 11 1
;; eit 2 10 2
;; eit 4 5 2
;; eit 4 4 8
;; eit 16 2 8
;; eit 256 1 8
;; eit 256 0 2048
;; 2048

;; Test
(display
 (list
  (fast-expt-iter 2 11)
  (fast-expt-iter 3 7)
  (fast-expt-iter 2 0)
  (fast-expt-iter 4 4)
  (fast-expt-iter 10 6)))
