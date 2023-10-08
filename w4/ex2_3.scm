#lang racket
(require berkeley)

; make a point (x, y)
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; make pairs of points
(define (make-pair p1 p2) (cons p1 p2))
(define (get-p1 pair) (car pair))
(define (get-p2 pair) (cdr pair))

; Implementation 1: rectangle is given by providing center,  length and width

(define (make-rectangle center length width) ; center is a point
  (define cx (x-point center))
  (define cy (y-point center))
  (let ( (TL (make-point (- cx (/ width 2)) (+ cy (/ length 2))))
         (TR (make-point (+ cx (/ width 2)) (+ cy (/ length 2))))
         (BL (make-point (- cx (/ width 2)) (- cy (/ length 2))))
         (BR (make-point (+ cx (/ width 2)) (- cy (/ length 2))))
         )
    (cons (make-pair TL TR) (make-pair BL BR))))



; Implementation 2: rectangle given by coordinates of two opposite corners
; A (x1,y1) bottom left, B (x2,y2) top right
(define (make-rectangle-with-corners BL TR)
  (let (
        (TL (make-point (x-point BL) (y-point TR)))
        (BR (make-point (x-point TR) (y-point BL)))
        )
    (cons (make-pair TL TR) (make-pair BL BR))
    ))


; the rectangle returned looks like
; [(x1,y1), (x2,y2)] [(x3,y3),(x4,y4)]

#|
(define (perimeter rect)
  (let (
        (x1 (car (car (car rect))))
        (y1 (cdr (car (car rect))))
        (x2 (car (cdr (car rect))))
        (y2 (cdr (cdr (car rect))))
        (x3 (car (car (cdr rect))))
        (y3 (cdr (car (cdr rect))))
        (x4 (car (cdr (cdr rect))))
        (y4 (cdr (cdr (cdr rect))))
        )
    (* 2
       (+
        (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))
        (sqrt (+ (square (- x4 x2)) (square (- y4 y2)))))
       )
    ))
|#

(define (get-dims rect)
  (let (
        (x1 (car (car (car rect))))
        (y1 (cdr (car (car rect))))
        (x2 (car (cdr (car rect))))
        (y2 (cdr (cdr (car rect))))
        (x3 (car (car (cdr rect))))
        (y3 (cdr (car (cdr rect))))
        (x4 (car (cdr (cdr rect))))
        (y4 (cdr (cdr (cdr rect))))
        )
    (cons (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))
          (sqrt (+ (square (- x4 x2)) (square (- y4 y2)))))
       
    ))


(define (perimeter rect)
  (let (
        (l (car (get-dims rect)))
        (w (cdr (get-dims rect)))
        )
    (* 2 (+ l w))
      ))

(define (area rect)
  (let (
        (l (car (get-dims rect)))
        (w (cdr (get-dims rect)))
        )
    (* l w)
      ))
  

(define center-point (make-point 0 0))
(define rectangle (make-rectangle center-point 6 4))
(display "Perimeter of rectangle: ")
(perimeter rectangle)
(display "Area of rectangle: ")
(area rectangle)

; We'll make the same rectangle using second method
(define rectangle2 (make-rectangle-with-corners (make-point -2 -3) (make-point 2 3)))
(display "Perimeter of rectangle 2: ")
(perimeter rectangle2)
(display "Area of rectangle 2: ")
(area rectangle2)


#|
(define (print-rectangle rectangle)
  (let* ((top-left (car (car rectangle)))
         (top-right (cdr (car rectangle)))
         (bottom-left (car (cdr rectangle)))
         (bottom-right (cdr (cdr rectangle))))
    (display "Top-Left: ")
    (display-point top-left)
    (newline)
    (display "Top-Right: ")
    (display-point top-right)
    (newline)
    (display "Bottom-Left: ")
    (display-point bottom-left)
    (newline)
    (display "Bottom-Right: ")
    (display-point bottom-right)
    (newline)))

(define (display-point point)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")"))

; Example usage:
(print-rectangle rectangle)
|#