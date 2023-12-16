#lang racket
(require berkeley)

#|
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) ;check the first element of first stream
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) ; apply proc to each first element
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define s1 (stream-enumerate-interval 1 5))
(define s2 (stream-enumerate-interval 6 10))
(define s3 (stream-map + s1 s2))
|#


(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(display "Run x 5")
(newline)

(stream-ref x 5)

(display "Run x 7")
(newline)

(stream-ref x 7)

#|
Notice `stream-map` uses `cons-stream` to construct its results. This means it return a stream.
A stream is a `cons` procedure whose car is available as value and its cdr is a delayed-object-procedure (a
promise for future evaluation).

When we call (define x (stream-map show (stream-enumerate-interval 0 10))) -> this produces a stream
whose car is 0 and its cdr is a promise.
The car 0 therefore can be used by `show` and `stream-map` can run its `(apply show 0)` and 0 is printed.
The cdr is a delayed obj so won't compute or display yet.

Now we call `(stream-ref x 5)`. This computes the values 0, 1 ... 5 to get to 5.
Since we now have 0 ... 5, they are no longer delayed objects and stream-map can show them. `stream-map` is now
halted at 5

Now when `(stream-ref x 7)` is called, 6 and 7 are computed by `stream-ref` so `stream-map` can proceed to print
6 and 7

|#