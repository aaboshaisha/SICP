#lang racket

;(define (get-range interval) (* (car interval) (/ (cdr interval) 100)))
;(define (compute-bounds n p)

(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))



  

