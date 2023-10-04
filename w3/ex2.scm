#lang racket
(require berkeley)

;A “perfect number” is defined as a number equal to the sum of all its factors less than itself.
;For example, the first perfect number is 6, because its factors are 1, 2, 3, and 6,
;and 1+2+3=6. The second perfect number is 28, because 1+2+4+7+14=28.
;What is the third perfect number? Write a procedure (next-perf n) that tests numbers
;starting with n and continuing with n+1, n+2, etc. until a perfect number is found.
;Then you can evaluate (next-perf 29) to solve the problem.
;Hint: you’ll need a sum-of-factors subprocedure.

(define (factorise n)
  (define (factors n div)
    (if (= n 1)
        1
        (if (= (modulo n div) 0)
            (se div
                (factors (/ n div) div))
            (factors n (+ 1 div))
            )
        ))
  (factors n 2))


;(factorise 6)
;(factorise 28)

; First: we define a function that given a number produces its factors
; version 2 - factoriZe version
; In version 1, we don't test all numbers from 1 to n to see if they are factors
; Version 1 does it more like what we do on paper. This is not what we need
(define (factorize n)
  (define (factors n div)
    (if (= div n)
        1 ; once we reach the end, always add 1 as factor
        (if (= (modulo n div) 0)
            (se div
                (factors n (+ 1 div)))
            (factors n (+ 1 div))
            )
        ))
  (factors n 2))

;(factorize 28)

;Next we define a function that sums a list of numbers
(define (sum-of-factors l)
  (if (empty? l)
      0
      (+ (first l) (sum-of-factors (butfirst l)))))

;(sum-of-factors '(1 2 3))
;(sum-of-factors (factorize 28))


; Finally we define the next-perf procedure 
(define (next-perf n)
  (if (= n (sum-of-factors (factorize n)))
      n
      (next-perf (+ n 1))))

(next-perf 2)
(next-perf 7)
(next-perf 29) ; produces correct answer 496
(next-perf 500) ; take a bit but gives correct answer 8128