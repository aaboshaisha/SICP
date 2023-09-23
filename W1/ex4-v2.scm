#lang racket
(require berkeley)

(define (ordered? lst)
  (cond ((empty? lst) #t) ;an empty list is ordered
        ((equal? (butfirst lst) '()) #t) ;a one item list is ordered
        ((and (<= (first lst) (first (butfirst lst))) (ordered? (butfirst lst)))) ;check each 2 numbers
        (else #f)))

