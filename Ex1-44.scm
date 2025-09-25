;; Previous exercise
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

;; Smoothing code
(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-n f n) ((repeated smooth n) f))

;; Tests
(define (square x) (* x x))

(display ((smooth square) 3.0))
(newline)
(display ((smooth-n square 5) 3.0))
(newline)
