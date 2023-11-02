#lang racket


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 0 (list 1 2 3))

(define (adjoin-set x set) (cons x set))
(define (union-set set1 set2) (append set1 set2))
(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define s1 (list 1 2 3))
(define s2 (list 1 3 4))

(union-set s1 s2)
(intersection-set s1 s2)
(adjoin-set 5 s1)