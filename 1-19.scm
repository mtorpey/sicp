;; One transformation is:
;; a := bq + aq + ap
;; b := bp + aq

;; Applying twice gives:
;; a := (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;    = b(2pq + qq) + a(pp + 2pq + 2qq)
;;    = b(2pq + qq) + a(2pq + qq) + a(pp + qq)
;; b := (bp + aq)p + (bq + aq + ap)q
;;    = b(pp + qq) + a(2pq + qq)
;; meaning we replace p with pp+qq and q with 2pq+qq

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(display
 (map fib (list 0 1 2 3 4 5 6 7 8 9 10 50 1000)))
