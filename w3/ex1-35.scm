#lang racket
(require berkeley)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1.0 (/ 1 x))) 1)



; x^2 = x + 1
; x = 1 + (1 / x)
