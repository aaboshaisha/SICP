#lang racket

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b)) ; single elements (not lists)
        ((or (and (empty? a) (not (empty? b))) (and (empty? b) (not (empty? a)))) #f) ;unequal lists
        ((and (empty? a) (empty? b)) #t) ; reaching end of both
        (else (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? (list 1 2 3) (list 1 2 3))
(equal? (list 1 2 3) (list 1 2))
(equal? 'a 'b)
(equal? 1 2)
(equal? 1 1)