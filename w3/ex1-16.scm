#lang racket
(require berkeley)

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;(expt 2 3)
;(expt 3 2)
;(expt 5 3)

(define (it-exp b n product)
  (if (= n 0)
      product
      (it-exp b (- n 1) (* b product))))

;(it-exp 2 3 1)
;(it-exp 3 2 1)
;(it-exp 5 3 1)

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

;(fast-exp 2 3)
;(fast-exp 4 2)


; ex1.16
;Design a procedure that evolves an iterative exponentiation process
;that uses successive squaring and uses a logarithmic number of steps, as does fast-exp

(define (f b n a)
  (cond ((= n 1) a)
        ((even? n) (f b (/ n 2) (* a (square b))))
        (else (f b (- n 1) (* b a)))))

(require racket/trace)
(trace f) ; the function to be traced 

(f 2 3 1)
;(f 3 2 1)
;(f 5 3 1)

(trace fast-exp)
(fast-exp 2 3)