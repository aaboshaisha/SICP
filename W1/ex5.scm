#lang racket
(require berkeley)

(define (e-at-end wd) ;checks if word ends with e
  (if (equal? (last wd) 'e)
      wd
      '())
  )

(define (ends-e s) ;iterates through the sentence and picks up only e-ending words
  (if (equal? s '())
      '()
      (se (e-at-end (first s))
          (ends-e (butfirst s)))))