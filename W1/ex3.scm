#lang racket
(require berkeley)

(define (replace w)
  (cond ((equal? w 'I) 'you)
        ((equal? w 'me) 'you)
        ((equal? w 'you) 'me)
        ((equal? w 'You) 'I)
        (else w))
  )

(define (switch s)
  (if (empty? s)
      '()
      (se (replace (first s))
          (switch (butfirst s)))
      )
  )


