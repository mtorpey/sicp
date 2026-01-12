#lang sicp
;;
;; Prerequisites
;;
(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(define (filter proc seq)
  (cond ((null? seq) nil)
        ((proc (car seq)) (cons (car seq) (filter proc (cdr seq))))
        (else (filter proc (cdr seq)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime? n)
  (define (find-divisor n test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b) (= (remainder b a) 0))
  (define (smallest-divisor n) (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;;
;; My function
;;
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;;
;; Simplifying prime-sum-pairs
;;
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;
;; Tests
;;
(display (unique-pairs 5))
(newline)
(display (prime-sum-pairs 10))
