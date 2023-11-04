#lang racket
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

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

              (display "This entry : ")
              (display this-entry)
              (newline)
             
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))


(require racket/trace)
(trace partial-tree)
;(list->tree (list 1))
;(list->tree (list 1 2))
;(list->tree (list 1 2 3))
(list->tree (list 1 2 3 4))


#|

Suppose we start with (list 1 2 3 4) ; n = 4
At each step, partial-tree splits it into:

left-size = (n - 1) / 2 = (4 - 1) / 2 = 1
right-size = n - (left-size + 1) = 4 - (1+1) = 2

this makes this list to -> (1) , (2), (3 4)
(2) becomes this-entry
(1) & (3 4) need to be processed by partial-tree to make left and right branches
(1) will be made to ((1) '() '())

(3 4) have n = 2
left-size = 0
right-size = 1
this means:
 this-entry : 3
 left-tree : '()
 right-tree: (partial-tree (4)) -> (4 '() '())

And so we get the final result

|#