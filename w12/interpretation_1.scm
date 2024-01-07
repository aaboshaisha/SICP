#lang racket
(require racket/trace)

#|
- We used abstraction to allow us to express "what" we think without worrying about "how" it's done.
- Eventually, we need some "process" to determine the "meaning" of expressions in our language.
- This process is "evaluation".
- We have seen one example of such a process which is the "environment model". It allowed us to determine the
meaning of expressions which in turn allowed us to unwrap expressions in terms of primitive operations. 
- Now we want to describe that process as a procedure.
- We want to build the evaluator as a procedure (not just abstract environment model)


Stages of interpretations:
eg expression "(average 4 (+ 5 5))"
1- Lexical analyzer: break up the string into "words" (tokens)
2- Parser:
 - convert the linear sequence of tokens to a tree:

        average (symbol)
        /     \
      4       + (symbol)
             / \
            5   5

Everytime we see "(" we create a tree structure. If it's already inside a tree, we create an internal tree.
Each horizontal slice through the tree corresponds to some combination.

3- Evaluator:
- take the tree + env -> interpret
(env acts like a dict: associates names and values)
- evaluator uses some rules to walk through this tree, look-up values associated with symbols in the env
and use the rules to convert complex expressions into simple things.

We'll start by trying to build an evaluator for arithmetic expressions such as:
(plus* 24 (plus* 5 6))
|#

#|
(define (eval* exp) ; case analysis (type checking) and type dispatch
  (cond ((number? exp) exp)
        ((sum? exp) (eval-sum exp)) ;noitce it's doing dispatch on type calling the appropriate proc for the exp
        (else (error "Unknown expression" exp))))


(define (eval-sum exp)
  (+ (eval* (cadr exp)) (eval* (caddr exp)))) ; go apply primitive '+' after evaling sub-expressions
;; (via recursively calling 'eval' on both argument expressions


(define (type-tag? exp sym)
  (eq? (car exp) sym))

(define (sum? exp)
  (and (pair? exp)
       (type-tag? exp 'plus*)))


;; testing
(trace eval*)
(trace eval-sum)
(define s '(plus* 24 (plus* 5 6)))

(eval* s)
|#

#|
Now, let's see we want to be able to define variables such as:
(define* x* (plus* 4 5)) ;
and be able to use it:
(plus* x* 2)

We find we need to have a way of storing bindings between names and values in a table

At the moment we'll use the ready-code of table implementation, lookup and insertion to focus on the interpreter
|#

;; table code
(define make-table make-hash)

(define (put! table key value)
  (hash-set! table key value))

(define (get table key)
  (hash-ref table key #f))
;; end table code

(define environment (make-table))

;; our evaluator needs to be updated to allow this behavior

#|
(define (eval* exp)
  (cond ((number? exp) exp)
        ((sum? exp) (eval-sum exp))
        ((symbol? exp) (lookup exp)) ; if it's a variable, lookup its value 
        ((define? exp) (eval-define exp)) ; if definition , dispatch appropriate behavior
        (else (error "Unknown expression type" exp))))
|#

;; define environment lookup
(define (lookup variable)
  (let ((binding (get environment variable)))
    (if (null? binding)
        (error "Unbouynd variable: " variable)
        binding)))

(define (eval-sum exp)
  (+ (eval* (cadr exp)) (eval* (caddr exp))))

(define (type-tag? exp sym)
  (eq? (car exp) sym))

(define (sum? exp)
  (and (pair? exp)
       (type-tag? exp 'plus*)))

;; new code
(define (define? exp)
  (and (pair? exp)
       (type-tag? exp 'define*)))

;; we'll use scheme's symbol?
(define (eval-define exp)
  (let ((key (cadr exp))
        (value-exp (caddr exp))) ;; extract key (symbol) and value (eval* subexpression)
    (put! environment key (eval* value-exp))
    'ok)) ;; design decision what to return

;; notice recursively calls eval* on subexp only
;; it doesn't eavl the first subexp ('define*) but treats it as symbol. This means we can have special forms.

#|
;; testing
(define s '(define* x* (plus* 4 5)))
(eval* s)
(eval* '(plus* x* 2))
|#

;; extending calculator to handle conditionals / branches
;; eg (if* (greater* y* 6) (plus* y* 2) 15)
;; (if <predicate> <consequent> <alternative>)

;; we've been rewriting the same type-hecking code over and over. Let's DRY
(define (tagged-list? exp sym)
  (and (pair? exp) (eq? (car exp) sym)))

(define (if? exp) (tagged-list? exp 'if*))

#|
(define (eval-if exp)
  (let ((if-predicate (cadr exp))
        (if-consequent (caddr exp))
        (if-alternative (cadddr exp)))
    (if (eq? true (eval* if-predicate))
        (eval* if-consequent)
        (eval* if-alternative))))
|#

;; we'll also need to evaluate expressions like 'greater*
(define (greater? exp) (tagged-list? exp 'greater*))
(define (eval-greater exp)
  (> (eval* (cadr exp)) (eval* (caddr exp))))

#|
;; update the eval*
(define (eval* exp)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp))
        ((sum? exp) (eval-sum exp))
        ((define? exp) (eval-define exp))
        ((greater? exp) (eval-greater exp))
        ((if? exp) (eval-if exp))
        (else (error "Unknown expression type" exp))))
|#

#|
;;testing
(trace eval-if)
(trace eval-greater)
(define s '(if* (greater* y* 6) (plus* y* 2) 15))
(eval* '(define* y* 10))
(eval* s)
|#

#|
Notice how operations like plus* and greater* are similar:
 - they evaluate all args of subexpressions
 - perform an operation on resulting values

We'll call this pattern: application

We want to implement a single case in 'eval*' for all applications

We'll do it as follows:
 - evaluate first subexpression (operation)
 - put name in env for each operation
 - value of the name is a procedure
 - apply operation to operands
|#


(define (eval* exp)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp))
        ((define? exp) (eval-define exp))
        ((if? exp) (eval-if exp))
        ((application? exp) (apply* (eval* (car exp)) ;; get the value of each piece (operators & operands)
                               (map eval* (cdr exp)))) ;; by using eval recursively then do sth to do the application
        (else (error "Unknown expression type" exp))))


(define (application? exp) (pair? exp)) ;; we can do this only because we would've checked all special-forms 1st

(define scheme-apply apply)
(define (scheme-primitive? exp) (tagged-list? exp 'primitive))
(define (get-primitive-operator exp) (cdr exp))

(define (apply* operator operands)
  (if (scheme-primitive? operator)
      (scheme-apply (get-primitive-operator operator) operands)
      (error "Unknow operator: " operator)))

;; apply checks if primitive procedure and if it is gets the corresponding primitive and applies it to args

;; put the primitives in environment

;(put! environment 'plus* (cons 'primitive +))
;(put! environment 'greater* (cons 'primitive >))
;(put! environment 'true (cons 'primitive #t))

;; we can make it a procedure
(define (make-primitive scheme-proc) (cons 'primitive scheme-proc))
(put! environment 'plus* (make-primitive +))
(put! environment 'greater* (make-primitive >))
(put! environment 'true* #t)


;; rewrite eval-if to check for errors in predicate 
(define (eval-if exp)
  (let ((predicate (cadr exp))
        (consequent (caddr exp))
        (alternative (cadddr exp)))
    (let ((test (eval* predicate)))
      (cond ((eq? test #t) (eval* consequent))
            ((eq? test #f) (eval* alternative))
            (else (error "predicate not a condition" predicate))))))

#|
;; testing
;(trace apply*)
;(trace eval*)
;(trace eval-if)
(eval* '(define* z* 9))
(eval* '(plus* 9 6))
(eval* '(if* true* 10 15))
|#


