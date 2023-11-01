#lang racket

; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((empty? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define s (list 1 2 3 4 5))
(element-of-set? 3 s)
(element-of-set? 6 s)

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 0 s)
(adjoin-set 1 s)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define s2 (list 1 4 5))

(intersection-set s s2)

; 2.59
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) set2)
        ((not (element-of-set? (car set1) set2)) (union-set (cdr set1) (cons (car set1) set2)))
        (else (union-set (cdr set1) set2))))

(union-set s s2)