#lang racket
(require berkeley)

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items)) (map f (cdr items))) ))

(map square (list 1 2 3 4))

(define (filter predicate? items)
  (cond ((null? items) nil)
        ((predicate? (car items)) (cons (car items) (filter predicate? (cdr items))))
        (else (filter predicate? (cdr items)))))
         
(filter odd? (list 1 2 3 4 5))

(define (sum-list items)
  (if (null? items)
      0
      (+ (car items) (sum-list (cdr items)))))

(sum-list (list 1 2 3 4))

(define (accumulate combiner initial items)
  (if (null? items)
      initial
      (combiner (car items) (accumulate combiner initial (cdr items)))))

(accumulate + 0 (list 1 2 3 4))