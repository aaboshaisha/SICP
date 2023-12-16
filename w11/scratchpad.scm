#lang racket

; implement streams with delayed eval cons
(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

; there can be an empty stream 
(define (stream-null? stream) (null? stream))
(define the-empty-stream '())

; implement their map / filter / accumulate procedures same as with lists

(define (stream-map proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc stream)
  (if (stream-null? stream) 'done
      (begin
        (proc (stream-car stream))
        (stream-for-each proc (stream-cdr stream)))))


(define (stream-enumerate-interval low high)
  (if (> low high) the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ 1 low) high))))


(define (display-line x)
  (display x)
  (newline))

(define (display-stream stream)
  (stream-for-each display-line stream)) 

; let's test these
(define (square x) (* x x))

(define s (stream-enumerate-interval 1 5))
(display-stream s)
(display-stream (stream-map square s))
(display-stream (stream-filter even? s))
