;; New cc procedure from book
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

;; Implementing the helper methods
(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;; Tests
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(display
 (cc 100 us-coins))

;;
;; The order of coin-values should not affect the result. In any case, we
;; explore a tree of possibilities and each way of making change will appear in
;; exactly one position in the tree.
;;
