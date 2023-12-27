#lang racket
(require racket/trace)

;; helpers ----->
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; ------->

; test expression
(define ex-con '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
                      ((assoc 'c '((d 1) (e 2))) => cadr)
                      (else false)
                      ))

(define (extended-cond-clauses exp) (cdr exp))
(define cls (extended-cond-clauses ex-con))

(define (extended-cond? clause)
  (and (pair? clause)
       (= (length clause) 3)
       (eq? '=> (cadr clause))))

(define cl (car cls))
(extended-cond? cl)


;(define (cond-test clause) (car clause)) ;; this is the same as the cond-predicate selector
;(cond-test cl)
 
(define (cond-predicate clause) (car clause)) ; is also the parameter for the cond-action / procedure

(define (cond-proc clause) (caddr clause)) ;; this is the action clause without argument 
(cond-proc cl)



(define tcond
  '(cond ((> 3 5) 'less-than-five)
         ((<= 7 10) 'less-than-or-equal-to-ten)
         ((= 5 5) 'equal-to-five)
         ((> 8 10) 'greater-than-ten)
         (else 'unknown)))


#|
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (extended-cond? first)
                (make-if (cond-predicate first)
                         (list (cond-proc first) (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
|#

;; this was the solution I came up with. Turns out there's a bug since (cond-predicate first) would be
;; evaluated twice and thus not necessarily to the same result.
;; The solution I got from CS61 notes was to transform (test => recipient) into a clause like:
#|
( (lambda (arg)
    (if arg
        (recipient arg)
        <expand-rest-of-clauses)) test )


So we only evaluate once
|#

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))


(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (extended-cond? first)
                (list (make-lambda 'foo
                                   (make-if 'foo
                                            (list (cond-proc first) 'foo)
                                            (expand-clauses rest))))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))



(define (cond-arrow-clause? clause)
  (and (pair? clause)
       (= (length clause) 3)
       (eq? (cadr clause) '=>)))

(define (cond-arrow-doer clause)
  (caddr clause))

(define env (make-base-namespace))

(eval (expand-clauses (cond-clauses tcond)) env)


(define expression
  '(cond ((> 3 5) => (lambda (_) 'less-than-five))
         ((<= 7 10) => (lambda (_) 'less-than-or-equal-to-ten))
         ((= 5 5) => (lambda (_) 'equal-to-five))
         ((> 8 10) => (lambda (_) 'greater-than-ten))
         (else 'unknown)))

;(trace expand-clauses)
(eval (expand-clauses (extended-cond-clauses expression)) env)

(eval '(cond ((assoc ''b '(('a 1) ('b 2))) => cadr) (else false)) env) 