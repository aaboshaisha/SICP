#lang racket
(require berkeley)

(sentence 'hello 'world)

; Exercise 1.2
(/ (+ 5
      4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
      (* 3
         (- 6 2)
         (- 2 7)))

(define (sum-squares x y)
  (+ (* x x) (* y y)))

; Exercise 1.3
(define (two-large a b c)
  (cond ((> a b c) (sum-squares a b))
        ((> a c b) (sum-squares a c))
        ((> b a c) (sum-squares b a))
        ((> b c a) (sum-squares b c))
        ((> c a b) (sum-squares c a))
        ((> c b a) (sum-squares c b))
        )
  )





;Ex 2 (HW) Write a procedure squares that takes a sentence of numbers as its argument
;and returns a sentence of the squares of the numbers:
(define (squares s)
  (if (empty? s)
      '()
      (se (square (first s))
          (squares (butfirst s)))
      ))

