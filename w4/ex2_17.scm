#lang racket
(require berkeley)

(define (last-pair lst)
  (if (empty? (cdr lst))
      (list (car lst))
      (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))
              