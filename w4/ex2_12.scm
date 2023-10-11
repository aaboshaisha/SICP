#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; define make-center-percent and percent
(define (compute-width center percent)
  (/ (* percent center) 100))

(define (make-center-percent center percent)
  (let ((width (/ (* percent center) 100)))
    (make-center-width center width)))

(define (percent i)
  (* 100 (/ (width i) (center i))))


;(compute-width 6.8 10)
;(make-center-percent 6.8 10)
;(define i (make-center-percent 6.8 10))
;(percent i)

; NB: This solution assumes center > 0 (here I think there's no need to do these computations
; If Resistance = 0
(define i (make-center-percent 10 50)) 
(lower-bound i) 
(upper-bound i) 
(center i)
(width i)
(percent i)
