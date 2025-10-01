;; From book
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; My procedure
(define (make-rat n d)
  (define (make-rat-inner n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (if (< d 0)
      (make-rat-inner (- n) (- d))
      (make-rat-inner n d)))

;; Tests
(print-rat (make-rat 3 2))
(print-rat (make-rat -1 3))
(print-rat (make-rat -6 -2))
(print-rat (make-rat 70 -7))
