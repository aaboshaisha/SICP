#lang racket
(define (cont-frac n d k)
  (if (= k 0)
      1.0
      (/ (n k)
         (+ (d k) (cont-frac n d (- k 1))  ))
      )) 



(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

;(require racket/trace)
;(trace cont-frac) ; the function to be traced
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           3)


; iterative version
(define (iter-frac n d k sum)
  (if (= k 0)
      sum
      (iter-frac n d (- k 1) (/ (n k) (+ (d k) sum)))))


(iter-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10 0)

(require racket/trace)
(trace iter-frac) ; the function to be traced
(iter-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           3 0)


