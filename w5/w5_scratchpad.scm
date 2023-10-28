#lang racket
(require berkeley)

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items)) (map f (cdr items))) ))

(map square (list 1 2 3 4))

(define (filter predicate? items)
  (cond ((null? items) nil)
        ((predicate? (car items)) (cons (car items) (filter predicate? (cdr items))))
        (else (filter predicate? (cdr items)))))
         
(filter odd? (list 1 2 3 4 5))

(define (sum-list items)
  (if (null? items)
      0
      (+ (car items) (sum-list (cdr items)))))

(sum-list (list 1 2 3 4))

(define (accumulate combiner initial items)
  (if (null? items)
      initial
      (combiner (car items) (accumulate combiner initial (cdr items)))))

(accumulate + 0 (list 1 2 3 4))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(enumerate-interval 1 10)

(define (enumerate-leaves tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-leaves (car tree))
                      (enumerate-leaves (cdr tree)))
              )))

(define tr (list 1 (list 2 (list 3 4) 5)))

(enumerate-leaves tr)


(define (pair-generator n)
  (accumulate append nil
  (map (lambda (i)
         (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n))))

(pair-generator 6)
(pair-generator 0)
(pair-generator 1)
(pair-generator 2)
(pair-generator 3)
(pair-generator 4)