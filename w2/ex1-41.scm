#lang racket
(require berkeley)

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ 1 x))

((double inc) 1)

(((double (double double)) inc) 5)

; explaination
; double (f) -> f(f) x
; (double double) -> dd x x 
; (double (double double)) -> double (dd) -> dd(dd) -> f(f(f(f)))
; we get f(f(f(f))) (inc(inc(inc(inc x)))) where each f is double
; douuble(double(double(double)))) (inc(inc(inc(inc))))
; 2 x 2 x 2 x 2 inc = 16 inc