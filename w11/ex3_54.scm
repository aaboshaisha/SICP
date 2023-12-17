#lang racket
(require berkeley)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

#|

fact(1) = 1 x fact(1)
fact(2) = 2 x fact(1)
fact(3) = 3 x fact(2)
...

Notice 1 multiplicand is integers (1 2 3 ....)
and 2nd is factorials

So put them in the function. 
|#