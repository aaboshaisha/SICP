#lang racket
(require berkeley)

(define (replace w)
  (cond ((equal? w 'I) 'you)
        ((equal? w 'i) 'you)
        ((equal? w 'me) 'you)
        ((equal? w 'you) 'me)
        ((equal? w 'You) 'I)
        (else w))
  )


(define (cap-first s)
  (if (equal? (first s) 'you) (se 'You (butfirst s))
      s))


(define (cswitch s)
  (if (empty? s)
      '()
      (se (replace (first s))
          (cswitch (butfirst s)))
      )
  )


(define (switch s)
  (cswitch (cap-first s)))


