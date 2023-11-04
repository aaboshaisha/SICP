#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))



(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (trace copy-to-list)
  (copy-to-list tree '()))


(define t
  (make-tree
    5
    (make-tree
      3
      (make-tree 1 '() '())
      '()
    )
    (make-tree
      9
      (make-tree 7 '() '())
      (make-tree 11 '() '())
    )
  )
)


(define t2
  (make-tree
    7
    (make-tree
      3
      (make-tree 1 '() '())
      (make-tree 5 '() '())
    )
    (make-tree 9 '() (make-tree 11 '() '()))
  )
)

(define t3
  (make-tree
    3
    (make-tree 1 '() '())
    (make-tree
      7
      (make-tree 5 '() '())
      (make-tree
        9
        '()
        (make-tree 11 '() '())
      )
    )
  )
)


t
t2
t3

(require racket/trace)
(trace tree->list-1)
;(trace tree->list-2)

;(tree->list-1 t2)
;(tree->list-2 t2)
(tree->list-1 t3)
(tree->list-2 t3)

#|
a- Both produce the same results
b- The recursive version (tree->list-1) does more work than the iterative version (tree->list-2).

|#