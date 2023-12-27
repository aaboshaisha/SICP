#lang racket

;; how the let* expression can be rewritten as nested lets:

(define lstar '(let* ((x 3)
                      (y (+ x 2))
                      (z (+ x y 5)))
                 (* x z)))

(define lnest '(let ((x 3))
                 (let ( (y (+ x 2) ))
                   (let ((z (+ x y 5)))
                     (* x z)))))


(define (let-clauses exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (make-let clauses body)
  (list 'let clauses body))


  
;(append (cadr lstar) (cddr lstar))

;(define l '(let ((x 3)) (x)))
;(define l2 (make-let (let-clauses l) (let-body l)))
;(equal? l l2)

#|
(define (let*->nested-lets exp)
  (if (null? (cdr exp))
      (car exp)
      (make-let (list (car exp)) (let*->nested-lets (cdr exp)))))

;; testing
(define l3 (let*->nested-lets (append (cadr lstar) (cddr lstar))))
|#

;; full version
(define (let*->nested-lets exp)
  (let ((flattened-exp (append (cadr exp) (cddr exp))))
    (define (iter items)
      (if (null? (cdr items))
          (car items)
          (make-let (list (car items)) (iter (cdr items)))))
    (iter flattened-exp)))

;; see if it does the desired conversion
(define ltest (let*->nested-lets lstar))
(equal? lnest ltest)
             
(define env (make-base-namespace))
(eval (let*->nested-lets lstar) env)
