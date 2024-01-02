#lang racket
(require racket/trace)


(define dl (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
;(define tr (list 1 (list 2 (list 3 (list 4))) (list 5 (list 6 7 8))))
(define tr '(7 (2 (1) (3)) (5 (4) (6))))

(define dl-sm (list (list 1 2) (list 3 4)))


;; treat both car and cdr same way
(define (deepmap f items)
  (cond ((null? items) '())
        ((not (pair? items)) (f items))
        (else (cons (deepmap f (car items))
                    (deepmap f (cdr items))))))


(define (square x) (* x x))
;(deepmap square tr)
;(deepmap square dl)


;; treat car and cdr differently

;; for deeplists
(define (map f items)
  (if (null? items)
      '()
      (cons (f (car items))
            (map f (cdr items)))))

(define (deepmap-1 f deeplist)
  (if (not (pair? deeplist))
      (f deeplist) ; apply func and return value/ explores inner lists
      (map (lambda (element) (deepmap-1 f element)) deeplist))) ;explores the outer list


;(trace deepmap-1)
;(trace map)
;(deepmap-1 square dl-sm)


;; for trees
;; tree has datum + lists (children) (car -> datum cdr->list)
(define (treemap f tree)
  (cons (f (car tree)) ; datum
        (forestmap f (cdr tree))))  ;list / children

(define (forestmap f items)
  (if (null? items)
      '()
      (cons (treemap f (car items)) ; datum
            (forestmap f (cdr items))))) ; children

;(trace treemap)
;(trace forestmap)
;(treemap square tr)


;; but we can do it in one procedure
(define (treemap-2 f tree)
  (cons (f (car tree)) ; apply f to datum
        (map (lambda (t) (treemap-2 f t)) (cdr tree)))) ; have map call treemap-2 on children

(treemap-2 square tr)


;;the two main tasks of treemap: 
;;- applying the function fn to one datum, and 
;; - using map to make a recursive call for each child. 