#lang racket
(require berkeley)

(define (square-list items) 
  (define (iter things answer) 
    (if (null? things) 
        answer 
        (iter (cdr things) 
              (cons (square (car things)) answer)))) 
  (iter items nil)) 
  
(square-list (list 1 2 3 4))


#|
This solution produces a reversed list because it cons the last item to the front of the list

Answer initially is nil
then it's (cons 1 nil)
and so on as follows
(cons 1 nil)
(cons 4 (cons 1 nil))
(cons 9 (cons 4 (cons 1 nil)))
etc
|#

(define (square-list2 items) 
  (define (iter things answer) 
    (if (null? things) 
        answer 
        (iter (cdr things) 
              (cons answer (square (car things)))))) 
  (iter items nil)) 
(square-list2 (list 1 2 3 4))

#|
This solution doesn't work because the answer it keeps building is a list then it tried to append
to its end. We've seen cons only works by appending to front

(cons nil 1)
(cons (cons nil 1) 4)
(cons (cons (cons nil 1) 4) 9)
etc
|#
