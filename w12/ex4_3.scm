#lang racket

; quick code for making table as global variable
(define the-get/put-table (make-hash))

(define (get key)
  (hash-ref the-get/put-table key #f))

(define (put key value)
  (hash-set! the-get/put-table key value)
  value)


;; Example usage
;; (put 'key (lambda (s) (* s s)))

;; To retrieve and apply the stored procedure
;; (get 'key))

(put 'quoted (lambda (exp env) (text-of-quotation exp)))
(put 'set! (lambda (exp env) (eval-assignment exp env)))
(put 'define (lambda (exp env) (eval-definition exp env)))
(put 'if (lambda (exp env) (eval-if exp env)))
(put 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
(put 'begin (lambda (exp env) (eval-sequence (beging-actions exp) env)))
(put 'cond (lambda (exp env) (eval (cond->if exp) env)))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (car exp)) ((get (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknow expression type --EVAL" exp))))


        
  
