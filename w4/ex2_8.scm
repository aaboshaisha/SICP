#lang racket
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))



; Subtracting intervals
#|
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
|#

; define subtraction as addition with negated second term a - b = a + (- b)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; when we negate interval y, the terms should be reversed
(define (negate-interval y)
  (make-interval (- 0 (upper-bound y)) (- 0 (lower-bound y))))

(define (sub-interval x y)
  (add-interval x (negate-interval y)))

; This gives the same formula as reversing the terms in the commented definition


  

