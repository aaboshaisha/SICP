#lang racket
(require berkeley)


(define (parity x) ; checks parity of x and returns its equivalent predicate
  (if (even? x) even? odd?))

;((parity 1) 1)
;((parity 1) 2)


(define (same-parity x . y)
  (define (iter y)
    (if (empty? y)
        nil
        (if ((parity x) (car y))
            (cons (car y) (iter (cdr y)))
            (iter (cdr y))
            )))
  (iter y))


(require racket/trace)
;(trace same-parity)
;(trace parity)



(same-parity 1 2 3 4 5 6 7 8 9 10)
(same-parity 2 2 3 4 6 8 10 22 11 33)


;((parity 1) (car (list 1 2 3 4)))

#| Notice in the solution we had to use an inner iterator which takes y as list input.
 The reason we can't directly implement this with same-parity as one function is if we pass
(same-parity x . (cdr y)) the dot '.' will remove the () from (cdr y) and cdr will be its first
argument so we get an error which says your parity function expects an integer not the procedure cdr
|#
