#lang racket
(require r5rs)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))



(count-pairs (list 1 2 3)) ; counts this fine
(count-pairs (cons 1 (cons 2 (cons 3 '())))) ; which is equivalent to this
(count-pairs (list 1 2 (cons 3 4))) ; fails here since it counts the last pair of the spine of the list
; which points to (cons 3 4) 
(count-pairs (list (cons 1 2) (cons 3 4) (list 5 6))) ; also fails here since now counts the pairs
; representing the spine of the list + the pairs that make its elements 

(define z (cons 7 8))
(define l (list 1 2 z))
(set-cdr! z l)
;(count-pairs z) ; goes to infinite loop

; it could also fails when pointers point to same elements
(define a 1)
(define b 2)
(define c 3)

(define l3 (list 1 2 3))
(set-car! (cdr l3) (cddr l3))
l3
(count-pairs l3)

(define h (list 1))
(define i (cons h h))
(define j (cons i i))
j
(count-pairs j)

(define p (cons 1 (cons 2 3)))
(count-pairs (list p p p))