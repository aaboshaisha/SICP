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

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v1 (list 1 2))
(define w1 (list 3 4))

(define v2 (list 1 2 3))
(define w2 (list 4 5 6))

(dot-product v1 w1)
(dot-product v2 w2)

(define m1 (list (list 1 2) (list 3 4)))
(define v3 (list 5 6))

#|
To perform matrix-vector multiplication

| 1  2 |   | 5 |   =   | (1*5 + 2*6) |
| 3  4 |   | 6 |       | (3*5 + 4*6) |

As u can see, we do:

(dot-product (row 1 of m) with v)
(dot-product (row 2 of m) with v)

We can achieve this with map but we need to replicate v for each row of m
|#

(define (copies v m) ; generate as many copies of v as length m
  (if (null? m)
      nil
      (cons v (copies v (cdr m)))))

(copies v3 m1)

; define matrix-vector multiplication
(define (matrix-*-vector m v)
  (map dot-product (copies v m) m))

(matrix-*-vector m1 v3)

; define transpose
(define (transpose mat)
  (accumulate-n cons nil mat))

(define m2 (list (list 1 2 3) (list 4 5 6)))
(transpose m2)

(define m3 (list (list 2 3) (list 4 5) (list 6 7)))
m3
(transpose m3)

; define matrix-matrix multiplication 
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map matrix-*-vector (copies cols m)  m)))



(define m4 (list (list 1 2) (list 3 4)))
(define m5 (list (list 5 6) (list 7 8)))

(matrix-*-matrix m4 m5)

(define m6 (list (list 1 2 3) (list 4 5 6)))
(define m7 (list (list 7 8) (list 9 10) (list 11 12)))

(matrix-*-matrix m6 m7)
