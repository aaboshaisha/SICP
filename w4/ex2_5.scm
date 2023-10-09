#lang racket

#|
Suppose n = 2^a + 3^b
If a = 2 and b = 3 then 2^3 + 3^2 = 72
How to find a and b if we have 72?
Keep dividing 72 by 2 till no longer divisible -> that is a
Keep dividing 72 by 3 till no longer divisible -> that is b
|#
(define (count-factors n f)
  (if (> (remainder n f) 0)
      0
      (+ 1 (count-factors (/ n f) f))))


;(count-factors 72 2)
;(count-factors 72 3)
;(* (expt 2 3) (expt 3 2))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (count-factors z 2))

(define (cdr z)
  (count-factors z 3))

(car (cons 2 3))
(cdr (cons 2 3))

#|
The key point here is to see that the same "data" type : "pair" can be represented
in many ways. We didn't necessarliy have to make it (a, b). Here we represented it
as one number (the product 2^a * 3^b) as long as we could have the constructors and selectors
(procedures) that satisfied the condition that when we (car pair) we get "a" and (cdr pair)
we get b
|#