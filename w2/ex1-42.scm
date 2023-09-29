#lang racket
(require berkeley)

; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ 1 x))
((compose square inc) 6)