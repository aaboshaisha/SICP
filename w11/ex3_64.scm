#lang racket
(require berkeley)

(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car (stream-cdr s)) (stream-car s))) tolerance)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) toelrance)))

