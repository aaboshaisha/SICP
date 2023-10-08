#lang racket
(require berkeley)

; Segment
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; Point
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (mid-segment segment)
  (make-point (/ (+ (x-point (end-segment segment)) (x-point (start-segment segment))) 2)
              (/ (+ (y-point (end-segment segment)) (y-point (start-segment segment))) 2)
              ))
; Printing
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")" ))


; Testing
(define p1 (make-point 1 4))
(define p2 (make-point 3 2))

(define segment (make-segment p1 p2))
(define midpoint (mid-segment segment))
(print-point midpoint)