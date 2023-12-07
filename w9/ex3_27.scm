#lang racket
(require r5rs)
(require racket/trace)

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))



(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


;where the memoizer is defined as

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result 
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

;The memoized version of the same procedure is

(define memo-fib
  (memoize 
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))


(define (fib-v2) (memoize fib))

(trace fib)
(trace memo-fib)
;(trace memoize)
;(fib 5)
;((fib-v2) 3)
(memo-fib 3)
(memo-fib 3)

#|
The original fib does tree recursion. It has redundant computations:

    fib(5)
    /     \
 fib(4)   fib(3)
 /   \      /   \
fib(3) fib(2) fib(2) fib(1)
  |     |     |     |
fib(2) fib(1) fib(1) fib(0)
  |     |
fib(1) fib(0)

memo-fib reduces these computations since once some value is computed for one branch, u don't need t
compute it for the other

memo-fib(5)
   |
   +-- memo-fib(4)
   |      \
   |       +-- memo-fib(3)
   |      /        \
   |     |          +-- memo-fib(2)
   |     |             \
   |     |              +-- memo-fib(1)
   |     |
   |      +-- memo-fib(2)
   |            \
   |             +-- memo-fib(1)
   |
   +-- memo-fib(3)
         \
          +-- memo-fib(2)
               \
                +-- memo-fib(1)


In this version, you can see that some of the subtrees are shared due to memoization.
When a value is computed for a specific argument, it is stored, and subsequent calls with the same
argument retrieve the result from the table.


If we however do the (fib-v2 version)

(fib-v2 5)
      |
      +--- fib-v2
              |
             memoize
                |
              make-table
                |
          (lambda (x) ...)
               \
                lookup
                 |
             (lambda (n) ...)
                      \
                     fib
                      |
               +------+------+
               |             |
              fib            fib
               |             |
          +---+---+     +---+---+
          |       |     |       |
         fib     fib     fib     fib
          |       |       |       |
     +---+---+ +---+---+ +---+---+ +---+---+
     |       | |       | |       | |       |
    fib     fib fib     fib fib     fib     fib
     |       |   |       |   |       |       |
   ...     ... ...     ... ...     ...     ...


The difference between memo-fib and fib-v2 is memo-fib calls the memoized version (in its recursive
calls) that has access to the table and can use it. fib-v2 however can either call lookup or fib
and fib calls itself not fib-v2 in the recursive calls. It doesn't make use therefore of the table
once it starts its recursive calls with fib.

|#