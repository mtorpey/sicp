(define (sum-sq x y)
  (+ (* x x) (* y y)))

(define (sum-sq-larger a b c)
  (if (< a b)
      (sum-sq b
              (if (< a c)
                  c
                  a))
      (sum-sq a
              (if (< b c)
                  c
                  b))))
