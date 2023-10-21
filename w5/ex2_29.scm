#lang racket
(require racket/trace)

#|
; definitions using (list )
(define (make-tree left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch tree) (car tree))
(define (right-branch tree) (car (cdr tree)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

|#


; definitions using (cons )
(define (make-tree left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cdr tree))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

; some initial testing: 
#|
(define lbr (make-branch 2 3))
(define rbr (make-branch 4 5))

(define tr (make-tree lbr rbr))

(display "get branches")
(newline)
(left-branch tr)
(right-branch tr)

(display "get branch structure")
(newline)
(branch-length (left-branch tr))
(branch-structure (right-branch tr))


(display "another tree")
(newline)
(define tr2 (make-tree lbr tr))
tr2

(left-branch tr2)
(right-branch tr2)
(left-branch (right-branch tr2))
(right-branch (right-branch tr2))

(display "test branch length and structure")
(newline)
(branch-length (left-branch (right-branch tr2)))
(branch-structure (right-branch (right-branch tr2)))
|#

#|
We're interested in accumulating weights of the tree.
The weight of the tree is = weight / branch-structure left-branch + weight right-branch
if branch-structure is number -> return it
else if branch-structure is tree -> call total-weight on it
else it's empty and we've reached the end so return 0

|#

(define (total-weight tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) tree)
        ((+ (total-weight (branch-structure (left-branch tree)))
            (total-weight (branch-structure (right-branch tree)))))))

(define a (make-tree (make-branch 2 3) (make-branch 4 5)))
(define d (make-tree (make-branch 10 a) (make-branch 12 5)))
d


(total-weight d)



(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? tree)
  (if (null? tree) #t
      (and (= (torque (left-branch tree)) (torque (right-branch tree)))
           (balanced? (branch-structure (left-branch tree)))
           (balanced? (branch-structure (right-branch tree))))))


(balanced? d)
           
