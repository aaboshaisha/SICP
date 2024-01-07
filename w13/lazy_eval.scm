#lang racket
(require racket/trace)
(require r5rs)

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

;; checking type-tags
(define (tagged-list? exp sym)
  (and (pair? exp) (eq? (car exp) sym)))

(define (if? exp) (tagged-list? exp 'if*))
(define (greater? exp) (tagged-list? exp 'greater*))
(define (define? exp) (tagged-list? exp 'define*))


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
    (let ((test (actual-value predicate env))) ;; changed
      (cond ((eq? test #t) (eval* consequent env))
            ((eq? test #f) (eval* alternative env))
            (else (error "predicate not a condition" predicate))))))



(define (application? exp) (pair? exp)) ;; we can do this only because we would've checked all special-forms 1st

(define apply-primitive-procedure apply)
(define (scheme-primitive? exp) (tagged-list? exp 'primitive))
(define (make-primitive scheme-proc) (cons 'primitive scheme-proc))
(define (get-primitive-operator exp) (cdr exp))


(define (lambda? exp) (tagged-list? exp 'lambda*))

(define (eval-lambda exp env)
  (let ((params (cadr exp)) ; get the pieces: params, body (expression), (env as input)
        (body (caddr exp))) ; body contains exactly one exp
    (make-compound params body env))) ; glue together with make-compound / create the double bubble of lambda*
;; the evaluation thus returns a list structure

;; constructors and selectors for compound procedure
(define (make-compound params body env)
  (list 'compound params body env))
(define (compound? exp) (tagged-list? exp 'compound))

(define (compound-params exp) (cadr exp))
(define (compound-body exp) (caddr exp))
(define (compound-env exp) (cadddr exp))



;; the core: eval <-> apply

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (eval* exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((define? exp) (eval-define exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (eval-lambda exp env))
        ((application? exp) (apply* (actual-value (operator exp) env) ;; changed: 
                                    (operands exp) ;; changed: just get the text of the expression
                                    env)) ;; changed: now passed as arg
        (else (error "Unknown expression type" exp))))

;; to implement lazy evaluation here, 2 changes to be made:
;; for primitive procedures: we need to force delayed values / get actual values
;; for compound procedures: we use delayed procedures

(define (apply* operator operands env) ;; changed: now env is an argument since we now have delayed args so we need to
  ;;remember where they came from
  (cond ((scheme-primitive? operator) (apply-primitive-procedure
                                       (get-primitive-operator operator)
                                       (list-of-arg-values operands env))) ;;changed: used to be just operands, here we
        ;; want to get actual values via "forcing" delayed values. 
        ((compound? operator)
         (eval* (compound-body operator) ; eval the body of procedure
                (extend-env-with-new-frame ; in a new env with bindings added to it
                 (compound-params operator) ;;procedure parameters
                 (list-of-delayed-args operands env) ;;changed: here also we change this to not get actual values but
                 ;;delayed ones
                 (compound-env operator)) ;; parent env
                ))
        (else (error "operator not a procedure: " operator))))

;; Thunks, delaying and forcing
(define (delay-it exp env) (list 'thunk exp env)) ;; just make it a tagged-list till needed

;; selectors
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))



#|
;; notice the mutual recursion between force-it and actual-value
(define (force-it obj)
  (cond ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))

;; Actual vs Delayed values
;; we now have 2 kinds of values
(define (actual-value exp env)
  (force-it (eval* exp env)))
;; we use force-it since eval* of some delayed-value may itself return a delayed values (in case of nested
;; expression) so we have to make sure we force each one till we finally get a value
|#


#|
we can memoize thunks to avoid repeat work:
How?
A thunk is just a list ('thunk exp env)
When evaluated (forced), we can "mutate" it to be ('evaluated-thunk result)
NB: we mutate (and not create a new one) so that if something else is pointing to that thunk, we don't have
to keep track of it and change it to the new one we created.
|#

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk) ; list element 1
           (set-car! (cdr obj) result) ; list element 2
           (set-cdr! (cdr obj) '()) ; list element 3
           result))
        ((evaluated-thunk? obj) (thunk-value obj)) ;; if already evaluated, just return its value
        (else obj)))
         
                         
(define (actual-value exp env)
  (force-it (eval* exp env)))

;; same pattern is used for list-of-arg-values and list-of-delayed-args
;; the only diff is the function applied (mapped): actual-value vs delay-it
(define (list-of-arg-values exps env)
  (if (null? exps)
      '()
      (cons (actual-value (car exps) env)
            (list-of-arg-values (cdr exps) env))))

(define (list-of-delayed-args exps env)
  (if (null? exps)
      '()
      (cons (delay-it (car exps) env)
            (list-of-delayed-args (cdr exps) env))))



;; testing

(define environment (make-table))
(put! environment 'plus* (make-primitive +))
(put! environment 'greater* (make-primitive >))
(put! environment 'true* #t)



(eval* '(plus* 6 9) environment)
(eval* '(define* z* 9) environment)
(eval* '(if* true* 10 15) environment)
(eval* '(if* (greater* 3 4) 10 15) environment)


;(trace eval*)
(eval* '(define* twice* (lambda* (x*) (plus* x* x*))) environment)
(eval* '(twice* 4) environment)
