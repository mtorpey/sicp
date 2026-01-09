#lang sicp

(define (sum-of-digits n base)
  (if (= n 0)
      0
      (let ((dividend (modulo n base)))
        (+ (sum-of-digits (/ (- n dividend) base) base) dividend))))

(define (square n) (* n n))

(define (quadratic-parent n base)
  (sum-of-digits (square n) base))

(define (index-of l x)
  (cond ((not (member x l)) -1)
        ((= x (car l)) 0)
        (else (+ 1 (index-of (cdr l) x)))))

(define (min-list l)
  (define (it l lowest)
    (if (null? l)
        lowest
        (it (cdr l) (min lowest (car l)))))
  (if (null? l)
      nil
      (it (cdr l) (car l))))

(define (uniq-cycles l min-length)
  (define (it l acc)
    (if (null? l)
        acc
        (let ((next (car l)))
          (if (or (member next acc) (not (= (car next) (min-list next))) (< (length next) min-length))
              (it (cdr l) acc)
              (it (cdr l) (cons next acc))))))
  (it l '()))

(define (lt-cycles a b)
  (< (car a) (car b)))

(define (sort l comp)
  (define (insert l x comp)
    (cond ((null? l) (list x))
          ((comp x (car l)) (cons x l))
          (else (cons (car l) (insert (cdr l) x comp)))))
  (define (it l comp acc)
    (if (null? l)
        acc
        (it (cdr l) comp (insert acc (car l) comp))))
  (it l comp nil))

(define (quadratic-cycle n base)
  (define (it n acc)
    (let ((next (quadratic-parent n base)))
      (let ((pos (index-of acc next)))
        (if (= pos -1)
            (it next (append acc (list next)))
            (list-tail acc pos)))))
  (it n (list n)))

(define (map-over-ints f min max)
  (define (it start)
    (if (> start max)
        '()
        (cons (f start) (it (+ start 1)))))
  (it min))

(define (all-quadratic-cycles base)
  (sort (uniq-cycles (map-over-ints (lambda (n) (quadratic-cycle n base)) 1 10000) 1) lt-cycles))

(define (all-nontrivial-quadratic-cycles base)
  (sort (uniq-cycles (map-over-ints (lambda (n) (quadratic-cycle n base)) 1 10000) 2) lt-cycles))

;(display
; (map-over-ints
;  (lambda (base) (list base (all-quadratic-cycles base)))
;  2 50))

;(newline)

(display
 (map-over-ints
  (lambda (base) (list base (all-nontrivial-quadratic-cycles base)))
  2 50))
