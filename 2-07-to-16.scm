#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2.1.4 Extended Exercise: Interval Arithmetic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Alyssa's first implementation
;;
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;;
;; Exercise 2.7
;; Completing the implementation
;;
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

;;
;; Exercise 2.8
;; Subtraction
;;
;; The minimum value of x-y is that where we minimise x and maximise y.
;; The maximum value of x-y is that where we maximise x and minimise y.
;;
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;
;; Exercise 2.9
;; Width
;;
;; An interval with lower bound l and upper bound u has width w=(u-l)/2.
;;
;; The sum of two intervals [l₁,u₁]+[l₂,u₂] is given by [l₁+l₂,u₁+u₂], so its
;; width is ((u₁+u₂)-(l₁+l₂))/2, which is (u₁-l₁)/2+(u₂-l₂)/2 = w₁+w₂.
;;
;; Similarly, the difference [l₁,u₁]-[l₂,u₂] is given by [l₁-u₂,u₁-l₂], so its
;; width is ((u₁-l₂)-(l₁-u₂))/2, which is (u₁-l₁)/2+(u₂-l₂)/2 = w₁+w₂.
;;
;; Hence the width of the sum or difference is a function of the widths of the
;; operand intervals. But this is not true for multiplication.
;;
;; Consider the product of two intervals each with width 1,
;;   [1,2]×[3,4]=[3,8],
;; which has width 2.5. But if we take another two intervals with width 1,
;;   [101,102]×[3,4]=[303,408],
;; we see that the width is 52.5, which is much higher.
;;
;; Similarly with division, consider these two quotients width-1 intervals:
;;   [1,2]/[3,4] = [0.25, 0.67]
;;   [101,102]/[3,4] = [25.25, 34.0]
;; which have different widths: 0.21 and 0.67 respectively.

;;
;; Exercise 2.10
;; Division by zero
;;
(define (div-interval-safe x y)
  (if (eq? (positive? (lower-bound y)) (positive? (upper-bound y)))
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))
      (error "Denominator interval contains zero" y)))

;;
;; Exercise 2.11
;; Optimising multiplication
;;
;; Each interval can be:
;;   - positive
;;   - straddling zero
;;   - negative
;; giving 3 cases for each interval, 9 cases overall. The "both straddling"
;; case is the only one where we need to try two products and compare them.
;;
(define (mul-interval-fast x y)
  (define (pos? x) (positive? (lower-bound x)))  ; above zero
  (define (npos? x) (not (positive? (upper-bound x))))  ; below (or touching from below) zero
  (let ((l1 (lower-bound x))
        (u1 (upper-bound x))
        (l2 (lower-bound y))
        (u2 (upper-bound y)))
    (cond ((pos?  x) (cond ((pos?  y) (make-interval (* l1 l2) (* u1 u2)))
                           ((npos? y) (make-interval (* u1 l2) (* l1 u2)))
                           (else      (make-interval (* u1 l2) (* u1 u2)))))
          ((npos? x) (cond ((pos?  y) (make-interval (* l1 u2) (* u1 l2)))
                           ((npos? y) (make-interval (* u1 u2) (* l1 l2)))
                           (else      (make-interval (* l1 u2) (* l1 l2)))))
          (else      (cond ((pos?  y) (make-interval (* l1 u2) (* u1 u2)))
                           ((npos? y) (make-interval (* u1 l2) (* l1 l2)))
                           (else      (make-interval
                                       (min (* l1 u2) (* u1 l2))
                                       (max (* l1 l2) (* u1 u2)))))))))
;;
;; Alyssa's center-width implementation
;;
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;
;; Exercise 2.12
;; Construction through percentage
;;
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))
(define (percent i)
  (* (/ (width i) (center i)) 100))

;;
;; Exercise 2.13
;; Tolerance of products
;; 
;; Two intervals a±x% and b±y% have upper bound
;;   (a + ax/100)(b + by/100) = ab + abx/100 + aby/100 + abxy/10000
;;                            = ab(1 + x/100 + y/100 + xy/10000)
;; If the tolerances x and y are small, xy is negligible in comparison to the
;; other terms, so we can omit the xy/10000 and get
;;   ab(1 + x% + y%),
;; giving us an approximate overall tolerance of (x+y)%.
;;

;;
;; Exercise 2.14
;;
;; No big mystery here. A product or quotient of two intervals has a width equal
;; to the sum of the two widths of the operands. So, each time we do these
;; operations we add more error.
;;
(define a (make-center-percent 10 0.01))
(define b (make-center-percent 20 0.05))
(display (list (div-interval-safe a a) (div-interval-safe a b)))

;;
;; Exercise 2.15
;;
;; Eva Lu Ator is right. One absurd conclusion of the effect mentioned above is
;; that a/a has some uncertainty to it. In reality it should be precisely 1, but
;; our algorithm has upper and lower bounds that use two *different* values of
;; a, not knowing that it should be a fixed (though unknown) constant.
;;
;; Avoiding repeats of variables helps with this, although it doesn't fix the
;; core problem…
;;

;;
;; Exercise 2.16
;;
;; The problem in general is that we're modelling an unknown using a range of
;; possible values, but not recognising cases where a single symbol is being
;; used multiple times in one expression. In such cases, we should never
;; evaluate the expression using both an upper and lower bound for that value,
;; but only consider evaluations where the symbol is replaced with a value
;; consistently.
;;
;; Doing this in general is much more complicated, but the right thing would be
;; to keep the expressions as purely symbolic without actually evaluating
;; anything until a value is needed (lazy evaluation). Then to evaluate the
;; expression, consider every combination of upper and lower bounds for the
;; variables, but *not* any combination that gives two occurrences of a variable
;; two different values.
;;
