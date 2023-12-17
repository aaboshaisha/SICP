#lang racket
(require berkeley)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (partial-sums s)
  (define partials (cons-stream (stream-car s) (add-streams partials (stream-cdr s))))
  partials)

(define s (partial-sums integers))

(define (print-stream-up-to-n stream n)
  (let loop ((s stream) (count 0))
    (cond ((and (not (stream-null? s)) (< count n))
           (display (stream-car s))
           (display " ")
           (loop (stream-cdr s) (+ count 1)))
          (else 'done))))

;; Example:
(print-stream-up-to-n s 5)

#|
This is very much like the factorial example except we do addition not multiplication.
We also don't start with 1 but could be any 1st element of a given stream.
|#