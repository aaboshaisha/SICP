#lang racket
(require berkeley)

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ( (next (improve guess)) )
      (if (good-enough? guess next)
          next
          (try next))))
  (lambda (first-guess) (try first-guess)))


(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 0.00001))
; test it
(close-enough? 1.0 1.00001)


(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)


(define (average x y)
  (/ (+ x y) 2.0))
; test it
(average 1 2)

(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (sqrt x)
  ((iterative-improve close-enough? (average-damp (lambda (y) (/ x y)))) 1.0))

(sqrt 9.0)

; The iterative-improve procedure could be written even simpler than above (via function composition)
(define (iterative-improve2 good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve2 good-enough? improve) (improve guess)))))

(define (fixed-point2 f first-guess)
  ((iterative-improve2 (lambda (x) (close-enough? x (f x))) f) first-guess))

(fixed-point2 cos 1.0)
