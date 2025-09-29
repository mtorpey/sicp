;;
;; Generic method
;;
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (display guess)
    (newline)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

;;
;; Apply to fixed-point and sqrt
;;
(define (fixed-point f first-guess)
  ((let ((tolerance 0.00001))
     (iterative-improve (lambda (guess)
                          (< (abs (- guess (f guess))) tolerance))
                        f))
  first-guess))

(define (sqrt x)
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2))
  ((let ((tolerance 0.001))
     (iterative-improve (lambda (guess) (< (abs (- (square guess) x)) tolerance))
                        (lambda (guess) (average guess (/ x guess)))))
   1.0))

;;
;; Tests
;;

;; From Ex 1.36: solve xˣ = 1000 via the transformation x ↦ log(1000) / log(x).
(display
 (fixed-point (lambda (x) (/ (log 1000) (log x)))
              2.0))
(newline)(newline)

;; Test sqrt
(display (sqrt 25.0))
(newline)
