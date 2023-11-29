#lang racket


(define f
  (let ((x 1))
    (lambda (inp)
      (set! x (- (expt x inp) inp)) x)))

(+ (f 0) (f 1))
(+ (f 1) (f 0))

#|
We want the call to f to depend on its history.
We give it an internal state x

If we make (x = x ^ input - x) then it will produce different results whether it saw 0 first or 1
|#

