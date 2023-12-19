#lang racket

(require berkeley)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(require racket/trace)

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s)
          (stream-cdr t))))

(trace pairs)

(define (print-stream-up-to-n stream n)
  (let loop ((s stream) (count 0))
    (cond ((and (not (stream-null? s)) (< count n))
           (display (stream-car s))
           (display " ")
           (loop (stream-cdr s) (+ count 1)))
          (else 'done))))

(define s (pairs integers integers))
(print-stream-up-to-n s 10)

#|
We'll see from tracing this it will run into an infinte loop.
When we call: (interleave s1 s2) our
s1 -> the stream from stream-map
s2 -> the stream from the call to pairs
pairs calls 2 cdrs -> no delay in it -> keeps incrementing both s and t infinitely.

|#


