#lang racket
(require berkeley)

#|
Extend the calculator program from lecture to include words as data,
providing the operations first, butfirst, last, butlast, and word.
Unlike Scheme, your calculator should treat words as self-evaluating expressions
except when seen as the operator of a compound expression.

That is, it should work like these examples:
calc: foo
foo
calc: (first foo)
f
calc: (first (butfirst hello)) e

|#


(define (accumulate proc initial items)
  (if (null? items)
      initial
      (proc (car items) (accumulate proc initial (cdr items)))))

;Applying procedure to args

;extending the program

(define (helper fn args)
  (if (null? args) (error "Calc: no args to " fn)
      (apply fn args)))



(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
        ((eq? fn '*) (accumulate * 1 args))
        ((eq? fn '-)
         (cond ((null? args) (error "Calc: no args to - "))
               ((= 1 (length args)) (- (car args)))
               (else (- (car args) (accumulate 0 + (cdr args))))))
        ((eq? fn '/)
         (cond ((null? args) (error "Calc: no args to / "))
               ((= 1 (length args)) (/ (car args)))
               (else (/ (car args) (accumulate * 1 (cdr args))))))

        ;extending the program
        ((eq? fn 'first) (helper first args))
        ((eq? fn 'last) (helper last args))
        ((eq? fn 'butfirst) (helper butfirst args))
        ((eq? fn 'butlast) (helper butlast args))
        ((eq? fn 'word) (helper word args))
        
        (else (error "Calc: Bad operator:" fn))))


(require racket/trace)

(define (calc-eval exp) ; translate expression in our "language" to computable values
  (cond ((number? exp) exp)
        ((symbol? exp) exp) ; extending the program
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp)))) ; procedure call
        (else (error "Calc: Bad expression:" exp))))

(trace calc-eval)
;(trace calc-apply)
;(trace helper)
;(calc-eval '(first foo))

(calc-eval '(+ (+ 3 4) (* 5 (+ 6 7))))


       
; The read-eval-print loop:


(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read))) ; read the exp user typed -> evaluate its value -> print it
  (newline)
  (calc))




                      