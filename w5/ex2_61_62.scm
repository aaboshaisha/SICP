#lang racket
; sets as ordered lists (numbers only)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;(element-of-set? 3 (list 1 2 3))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ( (x1 (car set1))
             (x2 (car set2)) )
        (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) (cdr set2)))
              ((> x1 x2) (intersection-set set1 (cdr set2)))))))

;(intersection-set (list 1 2 3) (list 1 2 4))
;(intersection-set (list 1 2 3) (list 5 6 4))


(define (adjoin-set x set)
  (cond ((null? set) (cons x '())) ; if we get to the end it means x is > all elements and should be added
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set)))))) ; if x > element of set, we need to keep these elements

;(adjoin-set 1 (list 1 2 4))
;(adjoin-set 5 (list 1 2 4))
;(adjoin-set 0 (list 1 2 4))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (if (< x1 x2)
               (cons x1 (union-set (cdr set1) set2))
               (union-set (cdr set1) set2))))))

(union-set (list 1 2 3) (list 1 3 4))
(union-set (list 1 2 3) (list 1 2 3))
(union-set (list 1 2 3) (list 4 5 6))
(union-set (list 1 2 3) '())
(union-set '() (list 1 2 3))
