#lang racket

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define lt '(let ((x 5)
                  (y 3))
              (+ x y)))

(define ld '((lambda (x y) (+ x y)) 5 3))


(define (let? exp) (tagged-list? exp 'let))

;; testing
;(let? lt)
;(let? ld)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

#|
;; some useful abstractions
(define (let-core exp) (cdr exp))
(define (let-clauses let-core) (car let-core))
(define (let-body let-core) (cdr let-core))
(define (let-var clause) (car clause))
(define (let-exp clause) (cadr clause))

(define c (car (let-clauses (let-core lt))))
(let-var c)
(let-exp c)

;(define (eval-let exp) (eval (let->combination (let-core exp) env)))

(define (let->combination let-core)
  (define (iter fn clauses)
    (if (null? clauses)
        '()
        (cons (fn (car clauses)) (iter fn (cdr clauses)))))

  (let ((body (let-body let-core))
        (clauses (let-clauses let-core)))
    (let ((vars (iter let-var clauses))
          (exps (iter let-exp clauses)))
      (cons (make-lambda vars body) exps))))


(define td (let->combination (let-core lt)))
|#

;; A better way to solve this is to do all the extraction outside the let->combinations and change the selectors
(define (let-vars exp) (map car (cadr exp))) ; one line to get variables
(define (let-exps exp) (map cadr (cadr exp))) ; one line to get expressions
(define (let-body exp) (cddr exp)) ; one line for the body

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp)) (let-exps exp)))

(define td (let->combination lt))

(equal? td ld) ;; returns true
