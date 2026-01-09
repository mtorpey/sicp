#lang sicp

;;
;; From book
;;
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

;; The value of
(display
 (fold-right / 1 (list 1 2 3)))
;; should be 1/(2/(3/1)) = 3/2
(newline)

;; The value of
(display
 (fold-left / 1 (list 1 2 3)))
;; should be ((1 / 1) / 2) / 3 = 1/6
(newline)

;; The value of
(display
 (fold-right list nil (list 1 2 3)))
;; should be (list 1 (list 2 (list 3 nil)))
;; which comes to '(1 (2 (3 ())))
(newline)

;; And the value of
(display
 (fold-left list nil (list 1 2 3)))
;; should be (list (list (list nil 1) 2) 3)
;; which comes to '(((() 1) 2) 3)
(newline)
