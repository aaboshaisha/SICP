#lang racket
(require berkeley)


(define (inc x) (+ x 1))
(define (identity x) x)
(define (pi-term x) (/ 1.0 (* x (+ x 2))))
(define (pi-next x) (+ x 4))
(define (cube x) (* x x x))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-ints a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (sum pi-term a pi-next b))

; we can rewrite pi-sum using lambda
(define (pi-sum2 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a (lambda (x) (+ x 4)) b))


; finding fixed points by repeatedly improving a guess till it satisfies some criterion

(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

























  