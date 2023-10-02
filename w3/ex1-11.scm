#lang racket
(define (f a b c n)
  (if (< n 3)
      a
      (f (+ a (* 2 b) (* 3 c)) a b (- n 1))))

(define (iter-f n)
  (if (< n 3)
      n
      (f 2 1 0 n)))

(iter-f 1)
(iter-f 3)
(iter-f 4)
(iter-f 5)

(define (rf n)
  (if (< n 3)
      n
      (+ (rf (- n 1))
         (* 2 (rf (- n 2)))
         (* 3 (rf (- n 3)))
         )))

(rf 1)
(rf 3)
(rf 4)
(rf 5)


(require racket/trace)
(trace f) ; the function to be traced 
(f 2 1 0 5) ; run the function with some argument