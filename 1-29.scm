(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (inner i)
    (if (= i 2)
        0
        (+ (* 4 (y (- i 1)))
           (* 2 (y (- i 2)))
           (inner (- i 2)))))
  (* (/ h 3) (+ (y 0)
                (* 4 (y 1))
                (inner n)
                (y n))))

;; Test
(define (cube x) (* x x x))
(display (list (integral cube 0.0 1.0 2)
               (integral cube 0.0 1.0 10)
               (integral cube 0.0 1.0 100)
               (integral cube 0.0 1.0 1000)))

;; Results indicate very quick convergence to 0.25
