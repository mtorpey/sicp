;;
;; I can't get the picture language Racket extension to work, so the following
;; code isn't functional. Let's quit here.
;;
(exit)


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.44 ;;
;;;;;;;;;;;;;;;;;;;
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.45 ;;
;;;;;;;;;;;;;;;;;;;
(define (split outer inner)
  (define (this-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (this-split painter (- n 1))))
          (outer painter (inner smaller smaller)))))
  this-split)


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.46 ;;
;;;;;;;;;;;;;;;;;;;
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.47 ;;
;;;;;;;;;;;;;;;;;;;

;; Using list
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

;; Using pairs
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.48 ;;
;;;;;;;;;;;;;;;;;;;
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.49 ;;
;;;;;;;;;;;;;;;;;;;
(define outline
  (let ((bl (make-vect 0 0))
        (tl (make-vect 0 1))
        (br (make-vect 1 0))
        (tr (make-vect 1 1)))
    (segments->painter (list (make-segment bl tl)
                             (make-segment tl tr)
                             (make-segment tr br)
                             (make-segment br bl)))))

(define cross
  (let ((bl (make-vect 0 0))
        (tl (make-vect 0 1))
        (br (make-vect 1 0))
        (tr (make-vect 1 1)))
    (segments->painter (list (make-segment bl tr)
                             (make-segment tl br)))))

(define diamond
  (let ((n (make-vect 0.5 1.0))
        (e (make-vect 1.0 0.5))
        (s (make-vect 0.5 0.0))
        (w (make-vect 0.0 0.5)))
    (segments->painter (list (make-segment s e)
                             (make-segment e n)
                             (make-segment n w)
                             (make-segment w s)))))

;; Skipped wave, which would involve creating many short segments to approximate
;; the 5 curves


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.50 ;;
;;;;;;;;;;;;;;;;;;;
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.51 ;;
;;;;;;;;;;;;;;;;;;;

;; Analogous to beside
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;; In terms of beside and rotations
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.52 ;;
;;;;;;;;;;;;;;;;;;;

;; a. Skipped, since we didn't bother with wave in Exercise 2.49

;; b. Simplify a bit, with just one stretched up and right picture
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top-left (up-split painter (- n 1)))
            (bottom-right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

;; c. Looking outward instead of inward when rogers is called
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside quarter (flip-horiz quarter))))
      (below (flip-vert half) half))))
