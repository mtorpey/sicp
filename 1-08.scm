(define (cbrt-iter guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (cbrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess last-guess)
  (< (/ (abs (- guess last-guess))
        guess)
     0.001))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (cbrt x)
  (cbrt-iter 1.0 2.0 x))

(display
 (map (compose cbrt cube)
      (list 0.000001 2 10 999999)))
