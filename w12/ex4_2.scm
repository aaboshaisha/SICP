#lang racket
#|
a.

application? is defined as (define (application? exp) (pair? exp))

Trying application first before cheking assignment means (define x 3) will be treated as application (since
it's a pair). This means the evaluator will try to evaluate its operands (x 3) and will not find x
instead of treating it as a symbol.


From the notes:
The trouble is that APPLICATION? cheats.  The book has

(define (application? exp) (pair? exp))

It really should be something like

(define (application? exp)
  (and (pair? exp)
       (not (member (car exp) '(quote set! define if lambda begin cond)))))

They get away with the shorter version precisely because EVAL doesn't
call APPLICATION? until after it's checked for all the possible special
forms.  Louis (quite reasonably, I think) wants to rely on APPLICATION?
behaving correctly no matter when it's called.
|#

;; b
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

