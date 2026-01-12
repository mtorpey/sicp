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

;;
;; From last exercise
;;
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;;
;; This exercise: we can cheat by calculating the only valid k from i and j
;;
(define (triples-that-sum n s)
  (let ((valid-pairs
         (filter (lambda (pair) (let ((total (+ (car pair) (cadr pair))))
                                  (and (< total s)
                                       (< (- s total) (cadr pair)))))
                 (unique-pairs n))))
    (map (lambda (pair) (append pair (list (- s (car pair) (cadr pair)))))
         valid-pairs)))

;;
;; Tests
;;
(display (triples-that-sum 5 9))
(newline)
(display (triples-that-sum 10 20))
