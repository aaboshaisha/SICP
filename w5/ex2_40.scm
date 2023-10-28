#lang racket
(require berkeley)

(define (accumulate combiner initial items)
  (if (null? items)
      initial
      (combiner (car items) (accumulate combiner initial (cdr items)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

(define (unique-pair n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pair 6)

; now we have sequence of pairs, we need to filter which pairs are prime
; predicate
(define (prime-sum? pair)
  (prime? (+ (car pair) (car (cdr pair)))))

(prime-sum? (list 1 2))
(prime-sum? (list 3 5))

; when we map over filtered sequence, we want to generate the triplet result for each pair (list i j i+j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(make-pair-sum (list 1 2))

(define (filter predicate? sequence)
  (cond ((null? sequence) nil)
        ((predicate? (car sequence)) (cons (car sequence) (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))


(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pair n))))

#|
We build this procedure inside out:

first generate the list of pairs: (unique-pair n)
then filter it to get only prime sum pairs: (filter prime-sum? (unique-pair n))
then generate triplets for each: (map make-pair-sum (filter prime-sum? (unique-pair n)))

|#

(prime-sum-pairs 6)