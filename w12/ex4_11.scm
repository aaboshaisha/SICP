#lang racket
(require r5rs) 

;; an environment is a list of frames
(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))


#|
; helper : recursively creates list of (key value) pairs
(define (iter vars vals)
  (if (null? vars)
      '()
      (cons (cons (car vars) (car vals)) (iter (cdr vars) (cdr vals)))))
;; a frame will be list of (k v) pairs (varibale value)
(define (make-frame variables values)
  (if (not (= (length variables) (length values)))
      (error "Number of variables and values don't match" variables values)
      (iter variables values)))

|#

;; a simpler implementation using map
(define (make-frame variables values)
  (if (not (= (length variables) (length values)))
      (error "Number of variables and values don't match" variables values)
      (cons 'frame (map cons variables values))))

(define (frame? f) (eq? (car f) 'frame))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))


(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))
;; remeber Scheme uses linked lists (spine and list of vals)
;; the car of the table (its head) is its first value (eg '(a . 1))
;; the cdr is the rest of values
;; this makes things tricky. It's better to use the separate header 'frame (same we did with table in Ch3)
;; to add to it, we attach the new entry to the cdr of the list and point the head (first value) to it


;; extending the environment doesn't change
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (contents frame) (cdr frame))


#|

(define (get var frame)
    (cond ((null? (frame-contents frame)) #f)
          ((pair? frame) (if (eq? var (car frame)) frame #f))
          ((eq? var (caar (frame-contents frame))) (car (frame-contents frame)))
          (else (get var (cdr frame)))))


(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (if (frame? env)
          (get var env)
          (let ((result (get var (first-frame env))))
            (if result
                result
                (lookup-variable-value var (enclosing-environment env)))))))
|#

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (let ((result (assoc var (contents frame))))
        (if result
            result
            (env-loop (enclosing-environment env)))))
    (cond ((eq? env the-empty-environment) (error "Unbound variable" var))
          ((frame? env) ; these let-lines to make it work for the 1d case and returns error instead of #f
           (let ((one-dim-result (assoc var (contents env))))
             (if one-dim-result
                 one-dim-result
                 (error "Unbound variable" env))))
          (else (scan (first-frame env)))))
  (env-loop env))


;; to set variable to new value we just do some minor modifications to lookup to make it change instead of
;; return the result
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (let ((result (assoc var (contents frame))))
        (if result
            (set-cdr! result val)
            (env-loop (enclosing-environment env)))))
    (cond ((eq? env the-empty-environment) (error "Unbound variable" var))
          ((frame? env) ; these let-lines to make it work for the 1d case and returns error instead of #f
           (let ((one-dim-result (assoc var (contents env))))
             (if one-dim-result
                 (set-cdr! one-dim-result val)
                 (error "Unbound variable" env))))
          (else (scan (first-frame env)))))
  (env-loop env))


;; finally, to define a variable
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan alist)
      (let ((result (assoc var alist)))
        (if result
            (set-cdr! result val)
            (add-binding-to-frame! var val alist))))

    (if (frame? env)
        (scan (contents env))
        (scan (contents frame)))
          ))

        


;; testing
(define f1 (make-frame '(a b c) '(1 2 3)))
;; testing
(add-binding-to-frame! 'd '4 f1)
(define f2 (extend-environment '(e f g) '(4 5 6) f1))
(define f3 (extend-environment '(h i j) '(7 8 9) f2))

(add-binding-to-frame! 'x '10 (car f2))


(lookup-variable-value 'a f1)
(lookup-variable-value 'a f2)
(lookup-variable-value 'a f3)
;(lookup-variable-value 'e f1)
(lookup-variable-value 'e f2)
(lookup-variable-value 'e f3)


;(get 'e (mcons 'd 4))

(set-variable-value! 'a 100 f1)
(set-variable-value! 'f 200 f2)
(set-variable-value! 'x 300 f3)


