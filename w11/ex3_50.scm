#lang racket
(require berkeley) ; We'll use the berkeley implementation of streams

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) ;check the first element of first stream
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) ; apply proc to each first element
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define s1 (stream-enumerate-interval 1 5))
(define s2 (stream-enumerate-interval 6 10))
(display-stream (stream-map + s1 s2))

; (map f items) -> applies f to each item in items (f item1) (f item2) .. etc
; so (map stream-car argstreams) -> produces a list of 1st elements of each stream in argstreams (a list of args)
; (map stream-cdr argstreams) -> produces a list of cdr elements of each stream (a list of args)
; the remaining code uses apply to apply the provided proc to all list given