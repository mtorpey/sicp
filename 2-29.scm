#lang sicp
;;
;; Helpers
;;
(define (compose f g) (lambda (x) (f (g x))))

;;
;; From book
;;
(define (make-mobile left right)
; (list left right)) ; replaced for (d)
  (cons left right))

(define (make-branch length structure)
; (list length structure)) ; replaced for (d)
  (cons length structure))

;;
;; a
;;

;; Components of a tree
(define left-branch car)
;(define right-branch cadr) ; replaced for (d)
(define right-branch cdr)

;; Components of a branch
(define branch-length car)
;(define branch-structure cadr) ; replaced for (d)
(define branch-structure cdr)

;;
;; b
;;
(define (total-weight mobile)
  (if (pair? mobile) ; is a mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile)) ; is a simple weight

;;
;; c
;;
(define (balanced? mobile)
  (if (pair? mobile) ; is a mobile
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and (= (* (branch-length left) (total-weight (branch-structure left)))
                (* (branch-length right) (total-weight (branch-structure right))))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))
      true)) ; simple weights are already balanced

;;
;; d
;;
;; Since total-weight and balanced? are implementation-agnostic (using only the
;; mobile-specific helper methods created in this exercise and not using car,
;; cdr and so on) there are almost no changes that need to be made.
;;
;; The head of a list and the first in a pair are the same: car. The second in a
;; pair is different from the tail of a list, so we need to use cdr instead of
;; cadr for right-branch and branch-structure.
;;
;; These changes are made above and the original version commented
;; out. Everything runs the same.

;;
;; Tests
;;

;; Unbalanced mobile m1
;;   /\
;; 2/  \5
;; 3  1/\2
;;    2  1
(define m1
  (make-mobile (make-branch 2 3)
               (make-branch 5 (make-mobile (make-branch 1 2)
                                           (make-branch 2 1)))))
(display (total-weight m1))
(display (balanced? m1))
(newline)

;; Balanced mobile m2
;;   /\
;; 2/  \4
;; 6  1/\2
;;    2  1
(define m2
  (make-mobile (make-branch 2 6)
               (make-branch 4 (make-mobile (make-branch 1 2)
                                           (make-branch 2 1)))))
(display (total-weight m2))
(display (balanced? m2))
