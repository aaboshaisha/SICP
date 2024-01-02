#lang racket
(require r5rs)
(require racket/trace)

;;; I re-did the whole thing in another file since I wasn't modifying env in-place 

;; starter code from the book
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; testing
(define f1 (make-frame '(a b c) '(1 2 3)))



;; my code
;; To remove a specific value from env, we can create a function that traverses the env
;; and constructs a new env with the desired elements removed.


(define (delete-pair! sym frame)
  (let ((vars (frame-variables frame))
        (vals (frame-values frame)))
    (define (helper vars vals new-vars new-vals)
      (cond ((null? vars) (cons new-vars new-vals))
            ((eq? sym (car vars)) (helper (cdr vars) (cdr vals) new-vars new-vals))
            (else (helper (cdr vars)
                          (cdr vals)
                          (cons (car vars) new-vars)
                          (cons (car vals) new-vals)))))
    (helper vars vals '() '())))

;;(define test-frame (delete-pair! 'c (cons '(a b c d e) '(1 2 3 4 5))))
;(frame-values test-frame)
;(frame-variables test-frame)

;; make-unbound! deletes the binding from all frames in env
(define (make-unbound! var env)
  (if (null? (enclosing-environment env))
      '()
      (cons (delete-pair! var (first-frame env))
            (delete-pair! var (enclosing-environment env)))))



(define f2 (extend-environment '(e f g) '(4 5 6) f1))
;(trace make-unbound!)
;(trace delete-pair!)
(define f3 (make-unbound! 'a f2))
;(first-frame f3)
;(enclosing-environment f3)
