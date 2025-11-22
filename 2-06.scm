#lang sicp

;; Helper
(define (compose f g) (lambda (x) (f (g x))))

;; 0 is represented by a function that returns the identity function, so 0 is
;;   f↦(x↦x)
(define zero
  (lambda (f)
    (lambda (x)
      x)))

;; n+1 is represented by
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;; 1 is represented by 0+1, which is
(lambda (f)
  (lambda (x)
    (f ((lambda (x) x) x))))
;; or
(lambda (f)
  (lambda (x)
    (f x)))

;; 2 is represented by 1+1 which is
(lambda (f)
  (lambda (x)
    (f ((lambda (x) (f x)) x))))
;; or
(lambda (f)
  (lambda (x)
    (f (f x))))

;; Overall, n is represented by a function that takes a function f and returns
;; the function fⁿ.
;;
;; That is, n is represented by
;;   f↦fⁿ
;; also written as
;;   f↦(x↦fⁿ(x))

;; m+n should be represented by
;;   f↦(x↦fᵐ(fⁿ(x)))
;; i.e. a function that takes f and returns a function that applies f n times
;; then applies f m times
(define (add-bad m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

;; We can also do this more functionally: m+n is represented by
;;   f↦fᵐ∘fⁿ
(define (add m n)
  (lambda (f)
    (compose (m f) (n f))))

;; To test this, we use a function f that adds 1 to its argument,
;;   f:x↦x+1
;; and we apply the result to x=0. This should map appropriately onto the
;; familiar integers.
(define three (add-1 (add-1 (add-1 zero))))
(define five (add-1 (add-1 (add-1 (add-1 (add-1 zero))))))

(define eight (add three five))

(display
 ((eight (lambda (x) (+ x 1)))
  0))
