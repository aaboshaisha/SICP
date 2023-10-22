#lang racket
(require berkeley)
(require racket/trace)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x)) rest)
                )
        )))

;(trace subsets)
(subsets (list 1 2 3))

#|
How to think about this and why it works?

We start with list
s -> (1 2 3)
(cdr s) : (2 3)
The subsets of this -> `rest` are ( () (2) (3) (2 3) )

What we want: add to this list the result of appending 1 (car s) to each item of this `rest` list and add them together
giving us a list made of `rest` + `appended_rest` where appended_rest looks like:
( (1) (1 2) (1 3) (1 2 3) )

We thus need a procedure that appends 1 to each item of the list ( () (2) (3) (2 3) )

The same if we have
s -> (2 3)
(cdr s) -> (3)
subsets of (cdr s) ie rest -> ( () (3) )
to append: (car s) -> 2


map applies a procedure to each item of a list
the appending procedure will be (lambda (list-to-append-to) (cons (missing item) list-to-append-to)))  

|#