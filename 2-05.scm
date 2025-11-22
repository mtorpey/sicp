#lang sicp

;;
;; New representation of a pair of non-negative ints by 2^a * 3^b.
;;
;; Racket doesn't seem to like overloading the procedure names cons, car and
;; cdr, so I've appended '-m' to each one.
;;
(define (cons-m a b)
  (* (expt 2 a) (expt 3 b)))

;; car-m and cdr-m have recursive processes
(define (car-m n)
  (if (= (remainder n 2) 0)
      (+ 1 (car-m (/ n 2)))
      0))

(define (cdr-m n)
  (if (= (remainder n 3) 0)
      (+ 1 (cdr-m (/ n 3)))
      0))

;; Tests
(define pair1 (cons-m 10 20))
(display (list pair1
               (car-m pair1)
               (cdr-m pair1)))
