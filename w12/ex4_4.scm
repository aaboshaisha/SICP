#lang racket
(require racket/trace)

(define (tagged-list? exp tag) (eq? (car exp) tag))
(define (and? exp) (tagged-list? exp 'and))
(define (begin-and exp) (cdr exp))

; in the eval loop
; (eval-and (begin-and exp) env)



(define (eval-and exps env)
  (cond ((null? exps) true)
        ((null? (cdr exps)) (eval (car exps) env))
        ((false? (eval (cadr exps) env)) false)
        (else (eval (cadr exps) env)
              (eval-and (cdr exps) env))
        ))

;; or is defined analogously but replace false with true
(define env (make-base-namespace))


;(trace eval-and)
(display "testing AND")
(newline)

(eval-and '(and #t #t) env)
(eval-and '(and #t #f #t) env)
(eval-and '(and #t #t #t #f #t) env)
(eval-and '(and #f #t #t) env)
(eval-and '(and #f #f) env)



(define (eval-or exps env)
  (cond ((null? exps) false)
        ((null? (cdr exps)) (eval (car exps) env))
        ((not (false? (eval (cadr exps) env))) true)
        (else (eval (cadr exps) env)
              (eval-or (cdr exps) env))
        ))

(display "testing OR")
(newline)

;(trace eval-or)
(eval-or '(or #t #t) env)
(eval-or '(or #f #f) env)
(eval-or '(or #f #f #t) env)
(eval-or '(or #t #f #f) env)
(eval-or '(or #t #f #t) env)

