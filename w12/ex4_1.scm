#lang racket

; 4.1
#| I initially thought that using iteration might solve this.
Turns out I was wrong.
Whether you are using iteration or recursion, the order of evaluation of arguments remains determined by
the applicative order principle.

|#

(define (list-of-values exps env)
  (define (iter lst items)
    (if (no-operands? exps)
        items
        (iter (eval (rest-operands exps) env) (eval (cons (first-operand exps) items) env))))
    (iter exps '()))




#|
Turns out, If you want to control the order of evaluation explicitly,
you can use begin or let to sequence the evaluations.
|#

(define (list-of-values exps env)
  (if (no-operantds? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (cons left (list-of-values (rest-operands exps) env)))))

; the R -> L version

; the R -> L version will work with reveresed list of exps

(define (list-of-values-r (reverse exps) env))

;or
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) right))))

