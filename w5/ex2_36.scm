#lang racket
(require berkeley)

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence) (accumulate op init (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list
           (list 1 2 3)
           (list 4 5 6)
           (list 7 8 9)
           (list 10 11 12)))

(accumulate-n + 0 s)

#|
How to come up with it?

accumulate should get the first item of each element of the subsequences.
get first of subsequence : car
to apply it to each subsequence of a list of subsequences : map

|#