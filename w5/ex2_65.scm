#lang racket


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
        ((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))


(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define s (list->tree (list 1 3 5 7 9)))

;(element-of-set? 3 s)
;(adjoin-set 1 (list->tree (list 2 3)))
;(adjoin-set 3 (list->tree (list 2 3)))

;(tree->list s)

; Use the results from 2.63 and 2.64 to implement O(n) union-set and intersetion set
#|
One naive solution:
We have already implemented O(n) union and intersection procedures for sets as ordered lists.
We start with a list that is a binary tree
We change it to ordered list using (tree->list-2 ) from 2.63 : O(n)
We do union / intersection for ordered lists : O(n)
We change back to balanced tree using (list->tree ) from 2.64 : O(log n)

This gives us: O(n) + O(n) + O(log n) => O(n)
|#

(define (union-list set1 set2)
  (define (union-iter set1 set2 result)
    (cond ((null? set1) (append result set2))
          ((null? set2) (append result set1))
          (else
           (let ((x1 (car set1))
                 (x2 (car set2)))
             (cond ((< x1 x2) (union-iter (cdr set1) set2 (cons x1 result)))
                   ((= x1 x2) (union-iter (cdr set1) (cdr set2) (cons x1 result)))
                   ((> x1 x2) (union-iter set1 (cdr set2) (cons x2 result))))))))
  (union-iter set1 set2 '()))

(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ( (x1 (car set1))
             (x2 (car set2)) )
        (cond ((= x1 x2) (cons x1 (intersection-list (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-list (cdr set1) (cdr set2)))
              ((> x1 x2) (intersection-list set1 (cdr set2)))))))


(define (union-set set1 set2)
  (let ( (lst1 (tree->list set1))
         (lst2 (tree->list set2)) )
    (list->tree (union-list lst1 lst2))))


(define s1 (list->tree (list 1 3 5 7)))
(define s2 (list->tree (list 2 4 6 8)))

(union-set s1 s2)

;(union-list (list 1 3 5 7) (list 2 4 6 8))