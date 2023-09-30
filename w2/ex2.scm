#lang racket
(require berkeley)

(define (every f s)
  (if (empty? s)
      '()
      (se (f (first s))
          (every f (butfirst s)))
      ))

(every square '(1 2 3 4))
(every first '(nowhere man))

; ex 3
(every (lambda (letter) (word letter letter)) 'purple)
(keep even? '(781 5 76 909 24))
(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)