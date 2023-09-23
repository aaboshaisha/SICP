#lang racket
(require berkeley)

(define (less-than s)
  (if (equal? (butfirst s) '())
      (< (first s))
  (< (first s) (first (butfirst s)))))


(define (ordered? s)
  (if (equal? (butfirst s) '())
      (less-than s)
      (and (less-than s) (ordered? (butfirst s)))
      ))