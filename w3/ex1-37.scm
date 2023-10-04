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
  (if (< k 0)
      sum
      (iter-frac n d (- k 1) (/ (n k) (+ (d k) sum)))))


(iter-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10 0)

(require racket/trace)
;(trace iter-frac) ; the function to be traced
(iter-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10 0)


; The recursive version isn't exctly correct. The reason it works here is because
; all n and d values are 1 so it doesn't make a huge difference. This only came to me
; when trying to make it work for exercise 1.38 which asks us to compute e using
; d values 1 2 1 1 4 1 1 6 1...etc. Here is the working version
; recursive version
(define (cont-frac n d k)
  (define (frac-iter i)
    (if (= i k)
        (/ (n i) (d i)) ; the last term is n_k / d_k
        (/ (n i) (+ (d i) (frac-iter (+ i 1)))) ; else statement
        ))
  (frac-iter 0))
