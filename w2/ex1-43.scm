#lang racket
(require berkeley)

; From 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 2)
      (compose f f)
      (compose f (repeated f (- n 1)) )))

; or we could do using identity
(define (repeated2 f n)
  (if (< n 1)
      (lambda (f) f)
      (compose f (repeated2 f (- n 1)))))

((repeated square 2) 5)
((repeated2 square 2) 5)