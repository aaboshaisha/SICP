#lang racket
(require berkeley)

(define (average a b)
  (/ (+ a b) 2.0))

(define (good-enough? v1 v2)
	(< (abs (- (square v1) v2)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x) x)))


; what is the new guess? average (x + x/y) where x/y = y = f
; suppose y = sqrt(x) ; f(x) = sqrt(x); y = f(x) ; y^2 = x ; y.y = x ; y = x/y
; f(x) = x/y; f(guess) = x/guess
; so what improve is doing is averging x and some f(x) = x/y
; the heart of sqrt procedure is just repeatedly applying f until it satisfies some criterion

(define (close-enough? v1 v2)
  (< (abs (-  v1 v2)) 0.00001))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; we can thus redefine sqrt as a repeat application of the fixed point procedure
(define (sqrt2 y x)
  (fixed-point (lambda (y) (average y (/ x y))) x))
; in this version, fixed-point takes a function (given by lambda expression) as input

; this functionality of averaging some x and some f(x) can be a procedure called average damping
(define (average-damp f)
  (lambda (x) (average x (f x))))
; average-damp is a procedure that takes as input a function f and returns a procedure that
; when applied to a number x it returns an average of x & f(x)

; we can now get v3 of sqrt
(define (sqrt3 y x)
  (fixed-point (average-damp (lambda (y) (/ x y))) x))









