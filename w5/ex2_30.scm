#lang racket
(require berkeley)

(define (square-tree-1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))
        ))

(define tr (list 1
                 (list 2 (list 3 4) 5)
                 (list 6 7)))

tr
(square-tree-1 tr)

(define (square-tree-2 tree)
  (map (lambda (subtree)
         (if (pair? subtree) (square-tree-2 subtree) (square subtree))) tree))

(square-tree-2 tr)