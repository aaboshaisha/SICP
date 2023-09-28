#lang racket
(require berkeley)

; version 1
(define (product1 a b)
  (if (> a b)
      1
      (* a (product1 (+ a 1) b))))

; version 2
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; example call
; (product2 (lambda (x) x) 1 (lambda (x) (+ 1 x)) 3)

(define (identity x) x)
(define (factorial n)
  (product identity 1 (lambda (x) (+ 1 x)) n))

(define (pi-term n)
  (/ (* 4.0 (square n)) (- (* 4.0 (square n)) 1)))

(define (inc x) (+ 1 x))

(define (wallis-prod a b)
  (product pi-term a inc b))

; this approach uses the formula from Wikipedia which gives pi / 2
; to get pi / 4
(define (pi-term2 n)
  (if (even? n)
      (/ (+ n 2.0) (+ n 1))
      (/ (+ n 1.0) (+ n 2))))

(define (pi-prod n)
  (product pi-term2 1 inc n))

