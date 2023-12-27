#lang racket

#|
The calculator program:

example:

calc: (+ 2 3)
5



The entire program consists of three procedures in 30 lines of code. You should find it easy to understand. And yet these three procedures exactly parallel the core procedures in a real Scheme interpreter:
1. The read-eval-print loop: interact with the user.
2. Eval: (eval expression) returns the value of the expression.
3. Apply: (apply function argument-values) calls the function and returns the value it returns.


So, it will:
1- Read user input
2- Evaluate subexpressions (exp -> value)
3- Apply procedure to arguments 
|#

(define (accumulate proc initial items)
  (if (null? items)
      initial
      (proc (car items) (accumulate proc initial (cdr items)))))


;(accumulate + 0 (list 1 2 3 4))


;Applying procedure to args

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
        ((eq? fn '*) (accumulate * 1 args))
        ((eq? fn '-)
         (cond ((null? args) (error "Calc: no args to - "))
               ((= 1 (length args)) (- (car args)))
               (else (- (car args) (accumulate 0 + (cdr args))))))
        ((eq? fn '/)
         (cond ((null? args) (error"Calc: no args to / "))
               ((= 1 (length args)) (/ (car args)))
               (else (/ (car args) (accumulate * 1 (cdr args))))))
        (else (error "Calc: Bad operator:" fn))))


; Evaluate expressions
#|
Expression typed by the user are represeneted as a list by scheme "read" which reads keyboard input from user.

The expression in the calculator program / language could be:
1- A number
2- A function (+ - / *) (procedure call represented by a list)


Scheme (langage - one that speaks to an interpreter) has 4 types of expressions:
1- Self-evaluating: numbers and booleans
2- Variables
3- Function calls
4- Special forms

In our calculator language, a user can type some expressions and we're writing an interpreter for them (calc.scm)
We have only 2 out of the 4 types above

|#


(define (calc-eval exp) ; translate expression in our "language" to computable values
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp)))) ; procedure call
        (else (error "Calc: Bad expression:" exp))))


       
; The read-eval-print loop:


(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read))) ; read the exp user typed -> evaluate its value -> print it
  (newline)
  (calc))



                      