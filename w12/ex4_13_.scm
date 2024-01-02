#lang racket
(require r5rs)
(require racket/trace)

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
      (list (make-frame vars vals) base-env) ; changes cons to list
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; testing
(define f1 (make-frame '(a b c) '(1 2 3)))

;; remove in-place from 1d list
;; lists in scheme are linked-lists
;; set-car! modifies the element and leaves spine intact
;; set-cdr! modifies spine
(define (remove-in-place item lst)
  (if (eq? item (cadr lst))
      (set-cdr! lst (cddr lst))
      (remove-in-place item (cdr lst))))
  
;; testing
;(define l '(1 2 3 4 5))
;(remove-in-place 3 l)

;; the key to in-place removal from linked-list is to recognize there are 2 cases:
;; - the element to remove is the first element
;; - the element to remove is NOT the first element
(define (unbind-in-frame sym frame)
  (define (remove-but-first-element vars vals) ; remove elements 2 -> end 
    (if (eq? sym (cadr vars))
        (begin
          (set-cdr! vars (cddr vars))
          (set-cdr! vals (cddr vals)))
        (remove-but-first-element (cdr vars) (cdr vals))))
  (trace remove-but-first-element)

  (if (eq? sym (car (frame-variables frame))) ; if sym is first element
      (begin
        (set-car! frame (cdr (frame-variables frame)))
        (set-cdr! frame (cdr (frame-values frame))))
      (remove-but-first-element (frame-variables frame) (frame-values frame) )))

;(define l (cons '(a b c d e) '(1 2 3 4 5)))
(define l (cons '(a b) '(1 2)))

;(trace unbind-in-frame)
(unbind-in-frame 'a l)
;(unbind-in-frame 'g l)


(define (env-iter sym env) ; this just iterates the proc above over all frames in env
  (cond ((eq? env the-empty-environment) 'done)
        ((memq sym (frame-variables (first-frame env)))
         (unbind-in-frame sym (first-frame env)))
        (else (env-iter sym (enclosing-environment env)))))



(define f3 (cons '(a b c) '(1 2 3)))
(define f4 (cons '(d e f) '(4 5 6)))
(define env-1 (list f3 f4))


(env-iter 'e env-1)


(define f2 (extend-environment '(e f g) '(4 5 6) f1)) 
(env-iter 'a f2)

