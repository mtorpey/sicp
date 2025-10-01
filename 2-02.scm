;; Line segments
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment s)
  (midpoint (start-segment s) (end-segment s)))

;; Points
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (midpoint p1 p2)
  (make-point
   (average (x-point p1) (x-point p2))
   (average (y-point p1) (y-point p2))))

;; Helpers
(define (average x y) (/ (+ x y) 2))

;; Printing (from book)
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;; Tests
(print-point
 (midpoint-segment
  (make-segment
   (make-point 0 0)
   (make-point 4 6))))

(print-point
 (midpoint-segment
  (make-segment
   (make-point -30 12)
   (make-point 10 -8))))

(print-point
 (midpoint-segment
  (make-segment
   (make-point -100 -150)
   (make-point -100 0))))
