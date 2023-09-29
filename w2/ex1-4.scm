#lang racket
(require berkeley)

; Newton's Method
;-----------------
; Suppose we have some function g(x) aka x |-> g(x)
; To find a "solution" to this function means to solve the equation g(x) = 0
; Newton's method says that a solution to g(x) = 0 can be found by finding fixed-point for function 
; f(x) aka x |-> f(x) where f(x) = x - (g(x) / Dg(x))
; A fixed point is where f(x) = x
; We find fixed points via repeated application of f: x, f(x), f(f(x)), f(f(f(x))), ...
; We start with initial guess for f(x): guess
; We find our next guess via applying f(guess) and we average guess & f(guess)
; (Notice that each guess is an application of f to some num and can be also called x
; and each next guess is f(f(that number)) and can be called f(x)
; we want the point where abs(guess - next) or abs(x - f(x)) is < 0.0001 (some threshold)

; let's implement these procedures
;----------------------------------

(define (close-enough? v1 v2)
  (< (abs(- v1 v2)) 0.00001))

(define (average x y)
  (/ 2.0 (+ x y)))

; we want to be able to take any function f(x) and fortify it such that
; it become a function that computes average of x and f(x) to any input x it takes
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f guess)
  (let ( (next (f guess)) )
    (if (close-enough? guess next)
        next
        (fixed-point f next))))

    
; derivative :  takes func as input and returns its deriv func
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
                 dx)))


(define (cube x) (* x x x))
( (deriv cube) 5)

; Netwton's method as fixed point process
; First part: Takes function g as input and returns func f(x) = x - (g(x) / g'(x))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))
    ))
; second part uses this in the fixed point procedure
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

; test with sqrt
(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0 ))

; Define a procedure cubic that can be used together with the newtons-method procedure
; in expressions of the form (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x^3 + ax^2 + bx + c

(define (cubic a b c)
  (lambda (x)
    (+
     (cube x)
     (* a (square x))
     (* b x)
     c)))

; test it
(define a 1)
(define b 1)
(define c 1)
(newton-method (cubic a b c) 1)
