#lang racket

#|
Developing a simple symbolic differentiation program
We want a procedure that given expression like ax^2 + bx + c and x |-> 2ax + b
We'll develop it initially to handle only + and * and 2 terms

The algorithm comes from the rules we know:

dc/dx = 0 if c is constant or c is variable but diff from x
dx/dx = 1
d(u+v) / dx = du/dx + dv/dx
d(u.v) / dx = u(dv/dx) + v(du/dx)

It will go like: (expression -> exp)

if exp is number -> 0
if exp is var:
  - is it same var as input var argument -> 1
  - else 0
if exp is sum -> (make-sum (deriv 1st-term-exp) (deriv 2nd-term exp))
if exp is prod -> (make-sum (make-prod 1st-term * (deriv 2nd-term)) (make-prod 2nd-term * (deriv 1st-term)))

This means our algorithm needs the following procedures:

is exp a number -> (number? exp)
is exp a variable -> (variable? exp) aka is it a symbol? 
are two inputs (variables or variable and number) same -> (same-variable? v1 v2)
is exp a sum -> (sum? x)
is exp a product -> (product? x)
(make-sum a1 a2)
(make-product m1 m2)

1st term of sum: addend
2nd term of sum: augend
1st term of prod: multiplicand
2nd term of prod: multiplier

We'll apply wishful thinking and assume we have these so we'll write the algorihtm and worry about representation
later
|#


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                                 (make-product (multiplicand exp) (deriv (multiplier exp) var))))
        ((exponentiation? exp) (make-product
                                (make-product (exponent exp)
                                              (make-exponentiation (base exp) (sub-exp (exponent exp) 1)))
                                (deriv (base exp) var)))
        (else (error "unknown expression type -- DERIV" exp))))



#|
The parts:
|#

; is expression a variable
(define (variable? exp) (symbol? exp))
; (variable? 1)
; (variable? 's)
;----------------
; are two inputs (variables or variable and number) same -> (same-variable? v1 v2)
;-> are the two input args of the same Type and same Value
(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
;(same-variable? 1 'a)
;(same-variable? 'a 'a)
;-----------------

; We'll use the prefix notation in building our expressions

; is exp a sum -> (sum? x)
; a sum is an expression whose first symbol is '+
(define (sum? exp) (eq? (car exp) '+))
;(sum? '(+ a1 a2))
;(sum? '(* a1 a2))
;----------------
; is exp a product -> (product? x)
(define (product? exp) (eq? (car exp) '*))
;----------------
; (make-sum a1 a2)
;(define (make-sum a1 a2) (list '+ a1 a2))
;(make-sum 'ax 'b)
;----------------
; (make-product m1 m2)
;(define (make-product m1 m2) (list '* m1 m2))
;----------------
; 1st term of sum: addend
(define (addend exp) (cadr exp))
;----------------
; 2nd term of sum: augend
(define (augend exp) (caddr exp))
;----------------
; 1st term of prod: multiplicand
(define (multiplicand exp) (cadr exp))
;----------------
; 2nd term of prod: multiplier
(define (multiplier exp) (caddr exp))
;----------------



; We want the program to reduce answers to their simplest terms such that
; x.0=0 , 1.y=y , 0+y=y
; We'll do this by modifying make-sum and make-product

(define (=number? exp num)
  (and (number? exp) (eq? exp num)))

(define (make-sum a1 a2)
  (cond ((=number? 0 a1) a2) ; if either is 0 return the other
        ((=number? 0 a2) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2)) ; if both numbers add them
        (else (list '+ a1 a2)))) ; otherwise they're to be treated as symbols

(define (make-product m1 m2)
  (cond ((=number? 1 m1) m2)
        ((=number? 1 m2) m1)
        ((or (=number? 0 m1) (=number? 0 m2)) 0)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; Ex 2.56

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((or (symbol? exponent) (symbol? base)) (list '** base exponent))
        (else (expt base exponent))))


(define (exponentiation? exp) (eq? (car exp) '**))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (sub-exp exp num)
  (cond ((and (symbol? exp) (pair? exp)) (list '- (cadr exp) (- (caddr exp) num)))
        ((symbol? exp) (list '- exp num))
        (else (- exp num))))


;(deriv '(+ x 3) 'x)
;(deriv '(* x y) 'x)
;(deriv '(* (* x y) (+ x 3)) 'x)


;(define e (make-exponentiation 'u 'n))

;(make-exponentiation 2 'n)
;(make-exponentiation 'u '2)
;(make-exponentiation 2 3)

(require racket/trace)

(deriv '(** u n) 'x)
(deriv '(** x 2) 'x)
(deriv '(** x n) 'x)

