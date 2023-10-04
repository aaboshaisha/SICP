#lang racket
(require berkeley)

; recursive version
(define (cont-frac n d k)
  (define (frac-iter i)
    (if (= i k)
        (/ (n i) (d i)) ; the last term is n_k / d_k
        (/ (n i) (+ (d i) (frac-iter (+ i 1)))) ; else statement
        ))
  (frac-iter 0))


; iterative version
; a_i = ni / (di + a_i-1)

(define (iter-cont-frac n d k sum)
  (if (< k 0)
      sum
      (iter-cont-frac n d (- k 1) (/ (n k) (+ (d k) sum)) )))

; to generate sequence 1 2 1 1 4 1 1 6 1...
; this function starts at i = 0 and detects positions 1,4,7, etc where
; multiples of 2 are

(define (d i)
  (if (= (remainder i 3) 1)
      (* 2 (+ 1 (quotient i 3)))
      1))

(require racket/trace)
;(trace iter)
(cont-frac (lambda (x) 1.0) d 10)
(iter-cont-frac (lambda (x) 1.0) d 10 0)


