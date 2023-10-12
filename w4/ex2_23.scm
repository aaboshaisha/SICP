#lang racket
(require berkeley)


(define (for-each f items)
  (define (iter items current)
    (if (null? items)
        (newline)
        (iter (cdr items) (f (car items)) )))
  (iter items nil))


(require racket/trace)
;(trace for-each)
(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4))


#|
The main problem we're trying to solve is we want to call 2 procedures:
- One will apply f(item)
- Other will do recursive call to for-each

In the solution above, we used an accumulator (current) in the inner function (iter) to
be the place where we call the f(item)

In the solution below, we use lambda to create our own custom procedure that does that trick
and it returns 2 calls as we need.
|#
(define (for-each-2 f items)
  (if (null? items)
      nil
      ( (lambda (lst) (f (car lst)) (for-each f lst)) items)))

(trace for-each-2)
(for-each-2 (lambda (x) (newline) (display x)) (list 1 2 3 4))
