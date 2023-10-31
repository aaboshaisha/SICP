#lang racket
(require berkeley)

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



(define (variable? exp) (symbol? exp))

(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

(define (sum? exp) (eq? (car exp) '+))
(define (product? exp) (eq? (car exp) '*))


(define (addend exp) (cadr exp))
(define (augend exp) (accumulate make-sum (cddr exp)))

(define (multiplicand exp) (cadr exp))
(define (multiplier exp) (accumulate make-sum (cddr exp)))


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


; Ex 2.57 handling arbitrary number of terms
;(define (make-sum a1 a2 . a3) (append (list '+ a1 a2) a3))
;(define (make-product a1 a2 . a3) (append (list '* a1 a2) a3))



;(make-sum '(x y (+ x 3)))

(deriv '(* x y (+ x 3)) 'x)



