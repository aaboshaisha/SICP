#lang racket
(require berkeley)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))


;
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map square (list 1 2 3 4))

; 
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

;
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4))

#|

How to think about all of these?

accumulate takes 3 arguments: op initial sequence
it applies op to (car sequence) and a recursive call to accumulate on the remainder of sequence

in map and length, you want op to return the appropriate accumulion procedure eg cons or +
and to apply the required transformation to x
and return the recursive call (y) as it is

in append: we already have the accumulation procedure cons
cons as we know can add items to the front of a list
we thus choose seq2 as initial value since we'll cons items one by one to its beginning
seq1 will be the sequence that accumulate itrates on one by one

|#