#lang racket
(require berkeley)

(define make-tree cons)
(define datum car)
(define children cdr)

(define (leaf? node) (null? (children node)))

(define t (make-tree 7 (list (make-tree 2 (make-tree 1 3)) (make-tree 5 (make-tree 4 6)))))
(define map-1 map)
t
;(datum t)
;(children t)
;(datum (children t))
;(children (children t))

; One way to map over trees with help of recursion:
; Trees are made of trees which are made of trees and so on.
; We reduce operations on trees to operations on branches which are reduced further till we get to leaves
(define (treemap-1 f tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (make-tree (treemap-1 f (datum tree)) (treemap-1 f (children tree))))))


; We'll test this with 2 different shapes of trees 
(treemap-1 (lambda (x) (* x x)) t)

(define (leaves . seq)
  (map (lambda (x) (make-tree x '())) seq))

(define t1 (make-tree 7
                      (list (make-tree 2 (leaves 1 3))
                            (make-tree 5 (leaves 4 6)))))

t1
(treemap-1 (lambda (x) (* x x)) t1)

#|
The other way is to regard a tree as a sequence of subtrees and use map
To think through it:
- What data structure should treemap return? Ans: tree so we'll use make-tree
- make-tree takes 2 args: datum and children, so we need to pass these to it
|#


(define (treemap fn tree)
  (make-tree (fn (datum tree))
             (map-1 (lambda (t) (treemap fn t))
                  (children tree) )))


(define (square x) (* x x))
(require racket/trace)
;(trace map-1)
;(trace treemap)
(treemap square t1)

(define (treemap-3 fn tree)
  (make-tree (fn (datum tree)) (forest-map fn (children tree))))

(define (forest-map fn forest)
  (if (null? forest)
      '()
      (cons (treemap-3 fn (car forest))
            (forest-map fn (cdr forest)))))

(treemap-3 square t1)

; Deep Lists: 
(define blist '(("john" "lennon") ("paul" "mccartney") ("george" "harrison") ("ringo" "starr")))

(define (deep-map f lol)
  (if (list? lol) (map (lambda (element) (deep-map f element)) lol)
      (f lol)))

(deep-map first blist)