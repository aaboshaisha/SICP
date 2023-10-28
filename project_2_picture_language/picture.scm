#lang racket
(require racket/gui)
(require racket/draw)


;Example usage of racket drawing tools
(define target (make-bitmap 100 100)) ; A 30x30 bitmap
(define dc (new bitmap-dc% [bitmap target]))
(send dc set-pen "red" 0.5 'solid)


#|
(send dc set-brush "green" 'solid)
(send dc set-pen "blue" 1 'solid)
(send dc draw-rectangle 0 10 30 10)
(send dc set-pen "red" 3 'solid)
(send dc draw-line 0 0 30 30)
(send dc draw-line 0 30 30 0)
|#

; (draw-line x1 y1 x2 y2)
(define sf 100)
(define (draw-line point1 point2)
  (send dc draw-line
        (* sf (car point1))
        (* sf (cdr point1))
        (* sf (car point2))
        (* sf (cdr point2))
        ))


;(draw-line (cons 0 0) (cons 1 1))
;(draw-line (cons 0 1) (cons 1 0))

;-----------STARTER CODE----------------------------
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ( (smaller (right-split painter (- n 1))) )
        (beside painter (below smaller smaller)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
;---------------------------------------
;Ex 2.50

(define (flip-horz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))


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

;Ex 2.51
(define (below painter1 painter2)
  (let ( (split-point (make-vect 0.0 0.5)) )
    (let ( (paint-down (transform-painter painter1
                                          (make-vect 0.0 0.0)
                                          (make-vect 1.0 0.0)
                                          split-point))
           (paint-up (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0)))
           )
      (lambda (frame) (paint-down frame) (paint-up frame)))))
                                          


;Ex 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ( (smaller (up-split painter (- n 1))) )
        (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ( (up (up-split painter (- n 1)))
             (right (right-split painter (- n 1))) )
        (let ( (top-left (beside up up))
               (bottom-right (below right right))
               (corner (corner-split painter (- n 1))) )
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;Ex 2.45
(define (split f1 f2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f1 f2) painter (- n 1))))
          (f1 painter (f2 smaller smaller))))))

; redefine 
(define right-split-2 (split beside below))
(define up-split-2 (split below beside))


;2.46 implement vectors and vector operations
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (let ( (x1 (xcor-vect v1))
         (x2 (xcor-vect v2))
         (y1 (ycor-vect v1))
         (y2 (ycor-vect v2)) )
    (make-vect (+ x1 x2) (+ y1 y2) )))

(define (sub-vect v1 v2)
  (let ( (x1 (xcor-vect v1))
         (x2 (xcor-vect v2))
         (y1 (ycor-vect v1))
         (y2 (ycor-vect v2)) )
    (make-vect (- x1 x2) (- y1 y2) )))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Test Examples
#|
; Test adding two vectors
(define v1 (make-vect 2 3))
(define v2 (make-vect 1 4))
(display "Addition: ")
(display (add-vect v1 v2)) ; Expected output: (3 7)
(newline)

; Test subtracting two vectors
(define v3 (make-vect 5 2))
(define v4 (make-vect 3 1))
(display "Subtraction: ")
(display (sub-vect v3 v4)) ; Expected output: (2 1)
(newline)

; Test scaling a vector
(define v5 (make-vect 3 4))
(define scale-factor 2)
(display "Scaling: ")
(display (scale-vect scale-factor v5)) ; Expected output: (6 8)
(newline)
|#


;Ex 2.47 Frame constructors and selectors
;using cons
#|
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (cdr (cdr frame)))
|#

;using list
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (car (cdr (cdr frame))))

; Testing
#|
(define v1 (make-vect 2 3))
(define v2 (make-vect 1 4))
(define v3 (make-vect 5 2))
(display "Testing frame selectors")
(newline)
(define frame1 (make-frame v1 v2 v3))
(origin-frame frame1)
(edge1-frame frame1)
(edge2-frame frame1)
|#


; frame-coord-map: returns lambda (V) |-> (Vf)
; returns a procedure that maps a vector in unit-square to vector in frame space
; using v = (x,y) = Origin(Frame) + x * Edge1(Frame) + y * Edge2(Frame)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame))))))


;Ex 2.48
(define (make-segment start-point end-point)
  (cons (make-vect 0 start-point) (make-vect 0 end-point)))

(define (start-segment segment) (cdr (car segment)))
(define (end-segment segment) (cdr (cdr segment)))


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment) (draw-line ((frame-coord-map frame) (start-segment segment))
                                  ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


; Testing if our frame procedures work
(define a-frame (make-frame (make-vect 1 1) (make-vect 3 2) (make-vect 0 3)))
;((frame-coord-map a-frame) (make-vect 0 0)) ; asks where (0,0) would be in frame vector space -> should return
; origin of frame which is (1,1)
;(origin-frame a-frame)
(define d-frame (make-frame (make-vect 0 0) (make-vect 100 0) (make-vect 0 100)))
(define u-frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))


; Ex 2.49: Implementing painters
;(draw-line point1 point2) -> decomposes to (draw-line x1 y1 x2 y2)
(define (x-painter frame)
  (let ((segment-list (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 0)))))
        ((segments->painter segment-list) frame)))


  
(define (diamond frame)
  (let ((segment-list (list
                       (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                       (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                       (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                       (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                       )))
        ((segments->painter segment-list) frame)))


(define (outline frame)
  (let ( (o (origin-frame frame))
         (e1 (edge1-frame frame))
         (e2 (edge2-frame frame)) )
    (define e3 (add-vect o (add-vect e1 e2))) ;Fourth point = Origin + e1 + e2
    (let ((segment-list (list
           (make-segment o e1)
           (make-segment o e2)
           (make-segment e2 e3)
           (make-segment e1 e3))))
      ((segments->painter segment-list) frame))))


; implementing wave
(define wave-list (list 
                       (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                       (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                       (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                       (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                       (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                       (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                       (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                       (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                       (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                       (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                       (make-segment (make-vect .4 1) (make-vect .6 1)) 
                       (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                       (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                       (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                       (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                       (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                       (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                       (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                       (make-segment (make-vect .75 0) (make-vect .6 0)) 
                       (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                       (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                       (make-segment (make-vect .4 0) (make-vect .25 0))))

(define (wave frame)
  ((segments->painter wave-list) frame))


;(wave u-frame)
;((flip-vert wave) u-frame)
;((below wave wave) u-frame)
;((beside diamond diamond) u-frame)
((corner-split (flip-vert wave) 4) u-frame)

;(x-painter u-frame)
;(outline u-frame)
;(diamond u-frame)
(make-object image-snip% target)