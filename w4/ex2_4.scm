#lang racket

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (cons x y)
  (lambda (m) (m x y)))


(car (cons 1 2))
(cdr (cons 1 2))


; cons is a procedure that takes another procedure and applies it to its arguments
; car is a procedure that takes procedure z and applies it to a procedure that returns the 1st of 2 args
; cdr is analogous

#|
Viewed with the substitution model:
(car (cons x y))
(car (lambda (m) (m x y)))

where: (lambda (m) (m x y)) is now z (in (car z))
Now we get:

((lambda (m) (m x y)) (lambda (p q) p)))
where: (lambda (p q) p) is the m in (m x y)

so we get: ((lambda (p q) p) x y) -> x

|#