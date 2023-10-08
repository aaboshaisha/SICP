#lang racket
(require berkeley)

(define (make-rat n d)
  (cond ((or (and (< n 0) (> d 0))
            (and (> n 0) (< d 0)))
         (cons (- 0 (abs n)) (abs d)))
         ((and (< n 0) (< d 0)) (cons (abs n) (abs d)))
         (else (cons n d))
         ))

        

(make-rat -1 3)
(make-rat 1 -3)
(make-rat -1 -3)
(make-rat 1 3)
(make-rat 0 -3)
(make-rat -3 0)