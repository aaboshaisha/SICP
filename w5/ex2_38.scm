#lang racket
(require berkeley)

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest)))) ; we store the computation in result - exapnds each time
    (iter initial sequence))


#|

Tracing fold-right is easy
To trace fold-left

for: (fold-left /  1 (list 1 2 3))

| result  | rest       | operation          |
| ------- | ---------- | ------------------- |
| 1       | (1 2 3)   | (/ 1 1) = 1        |
| 1       | (2 3)     | (/ 1 2) = 1/2      |
| 1/2     | (3)       | (/ 1/2 3) = 1/6    |

Same for list as op and nil initial
|#
(fold-right / 1 (list 1 2 3))
(fold-left /  1 (list 1 2 3))

(fold-right list nil (list 1 2 3))
(fold-left list  nil (list 1 2 3))

#| Property that op should satisfy such that both fold-reight and fold-left give same result:
Commutative 
|#
; examples

(fold-right * 1 (list 1 2 3))
(fold-left *  1 (list 1 2 3))

(fold-right + 1 (list 1 2 3))
(fold-left +  1 (list 1 2 3))