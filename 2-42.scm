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
;; Procedure with nested procedures
;;
(define (queens board-size)

  ;; A board is a list of n ints, one for each column
  (define empty-board nil)

  ;; Always append a new queen to the left (ignore k)
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

  ;; Check whether the first position clashes with any others
  (define (safe? k positions)
    ;; Check if the new queen (which is *distance* columns to the left) clashes
    (define (safe-with-remaining-cols? new-row distance positions)
      (if (null? positions)
          true
          (let ((row-to-check (car positions)))
            (if (or (= new-row row-to-check)                ; Horizontal move
                    (= new-row (+ row-to-check distance))   ; Diagonal up move
                    (= new-row (- row-to-check distance)))  ; Diagonal down move
                false
                (safe-with-remaining-cols? new-row (+ distance 1) (cdr positions))))))

    (let ((new-row (car positions)))
      (safe-with-remaining-cols? new-row 1 (cdr positions))))
    
  
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;
;; All 4 solutions with 6 queens
;;
(display (queens 6))
(newline)

;;
;; Calculate number of solutions for board sizes, which should match https://oeis.org/A000170
;;
(display
 (map (lambda (board-size)
        (length (queens board-size)))
      (enumerate-interval 0 12)))
