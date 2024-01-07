#lang racket
(require racket/trace)
(require r5rs)

;; Environment as explicit parameter
;; all procedures that call eval* will have an extra argument env
;; lookup and define (put) will use env from argument


;; table code
#|
(define make-table make-hash)

(define (put! table key value)
  (hash-set! table key value))

(define (get table key)
  (hash-ref table key #f))
;; end table code
|#


;; checking type-tags
(define (tagged-list? exp sym)
  (and (pair? exp) (eq? (car exp) sym)))

(define (if? exp) (tagged-list? exp 'if*))
(define (greater? exp) (tagged-list? exp 'greater*))
(define (define? exp) (tagged-list? exp 'define*))

#|
;; define environment lookup
(define (lookup variable env)
  (let ((binding (get env variable)))
    (if (null? binding)
        (error "Unbouynd variable: " variable)
        binding)))



(define (eval* exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((define? exp) (eval-define exp env))
        ((if? exp) (eval-if exp env))
        ((application? exp) (apply* (eval* (car exp) env) 
                               (map (lambda (expression) (eval* expression env)) (cdr exp)))) ;;the only non-trivial case
        (else (error "Unknown expression type" exp))))
|#

;; get the value of each piece (operators & operands)
;; by using eval recursively then do sth to do the application

(define (eval-define exp env)
  (let ((key (cadr exp))
        (value-exp (caddr exp))) ;; extract key (symbol) and value (eval* subexpression)
    (put! env key (eval* value-exp env))
    'ok)) ;; design decision what to return


(define (eval-greater exp)
  (> (eval* (cadr exp)) (eval* (caddr exp))))

(define (eval-if exp env)
  (let ((predicate (cadr exp))
        (consequent (caddr exp))
        (alternative (cadddr exp)))
    (let ((test (eval* predicate env)))
      (cond ((eq? test #t) (eval* consequent env))
            ((eq? test #f) (eval* alternative env))
            (else (error "predicate not a condition" predicate))))))



(define (application? exp) (pair? exp)) ;; we can do this only because we would've checked all special-forms 1st

(define scheme-apply apply)
(define (scheme-primitive? exp) (tagged-list? exp 'primitive))
(define (make-primitive scheme-proc) (cons 'primitive scheme-proc))
(define (get-primitive-operator exp) (cdr exp))


#|
(define (apply* operator operands)
  (if (scheme-primitive? operator)
      (scheme-apply (get-primitive-operator operator) operands)
      (error "Unknow operator: " operator)))



;; test our changes
(define environment (make-table))
(put! environment 'plus* (make-primitive +))
(put! environment 'greater* (make-primitive >))
(put! environment 'true* #t)

(eval* '(plus* 6 9) environment)
(eval* '(define* z* 9) environment)
(eval* '(if* true* 10 15) environment)
|#

;; Now we want to add the ability to create new procedures
;; eg (define* twice* (lambda* (x*) (plus* x* x*)))
;; (twice* 4)
#|
Strategy:
- Add a case for lambda* to eval
  -> the value of lambda* is a compound procedure, so
- Extend apply* to handle compund procedures
- Complete the implementation of the environment model
|#

;; add case for lambda* to eval
(define (eval* exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((define? exp) (eval-define exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (eval-lambda exp env)) ;; Add a case for lambda* to eval
        ((application? exp) (apply* (eval* (car exp) env) 
                               (map
                                (lambda (expression) (eval* expression env))
                                (cdr exp)))) ;;the only non-trivial case
        (else (error "Unknown expression type" exp))))

(define (lambda? exp) (tagged-list? exp 'lambda*))
;; (lambda* <params> <body>)

#| Remeber from the environment model:
To apply a compound procedure P to args:
1- Create a new frame A
2- Make frame “A” into an Environment by providing it with a pointer to its enclosing env.
   Its encolsing env will be the SAME frame of the procedure P.
3- In A, bind params of P to arg values.
4- EVAL body of P with E as the current env (notice this is a recursive step).
|#

;; (pull the pieces of the tree)
;; create the double bubble of lambda*
;; make it a compound procedure type 
(define (eval-lambda exp env)
  (let ((params (cadr exp)) ; get the pieces: params, body (expression), (env as input)
        (body (caddr exp))) ; body contains exactly one exp
    (make-compound params body env))) ; glue them together with make-compound

;; the evaluation thus returns a list structure

;; constructors and selectors for compound procedure
(define (make-compound params body env)
  (list 'compound params body env))
(define (compound? exp) (tagged-list? exp 'compound))

(define (compound-params exp) (cadr exp))
(define (compound-body exp) (caddr exp))
(define (compound-env exp) (cadddr exp))


;; environment model updated

(define (make-table) (list 'empty))

(define (put! table key value)
  (if (eq? (car table) 'empty)
      (set-car! table (cons key value))
      (let ((record (assoc key table)))
        (if record
            (set-cdr! record value) ; if record exists, just change its value / cdr
            (set-cdr! table (cons (cons key value) (cdr table)))
            ))))
        ;; otherwise attach the new pair to the beginning of the skeleton of the alist (table)

;; to fill table with list of keys and vals
(define (mput! table keys vals)
  (if (= (length keys) (length vals))
      (if (null? keys)
          'done
          (begin
            (put! table (car keys) (car vals))
            (mput! table (cdr keys) (cdr vals))))
      (error "Length of keys and values mismatch: " (length keys) (length vals))))

(define (table? t) (not (pair? (caar t))))


(define (lookup key env)
  ;; This functionality has to be a mutual recursion function since we have to treat
  ;; elements of this 2D structure differently 
  (define (env-loop env)

    (define (search key table)
      (cond ((null? table) (env-loop (cdr env))) ; when it has explored one table, go to next
            ((assoc key table) (cdr (assoc key table)))
            (else (search key (cdr table)))))
  
    (cond ((null? env) (error "Unbound variable: " key))
          ((table? env) (search key env)) ; case when env is 1 frame
          (else
           (let ((table (car env)))
             (search key table)))))
    
  (env-loop env)
  )


(define (extend-env-with-new-frame keys vals env)
  (let ((current-env (make-table)))
    (mput! current-env keys vals)
    (cons current-env env)))
;; end of env model

;; Extend apply* to handle compund procedures

(define (apply* operator operands)
  (cond ((scheme-primitive? operator) (scheme-apply (get-primitive-operator operator) operands))
        ((compound? operator)
         (eval* (compound-body operator)
                (extend-env-with-new-frame
                 (compound-params operator) operands (compound-env operator))
                ))
        (else (error "operator not a procedure: " operator))))


;; testing

(define environment (make-table))
(put! environment 'plus* (make-primitive +))
(put! environment 'greater* (make-primitive >))
(put! environment 'true* #t)



(eval* '(plus* 6 9) environment)
(eval* '(define* z* 9) environment)
(eval* '(if* true* 10 15) environment)

;(trace eval*)
(eval* '(define* twice* (lambda* (x*) (plus* x* x*))) environment)
(eval* '(twice* 4) environment)


#|
Notice the mutual recursion between eval and apply at the core of the evaluator:
- eval calls one of many eval-x helpers that each call eval to unwind its expressions till we get down
to primitives on which it calls apply

- apply calls eval with expressions and env 
|#
