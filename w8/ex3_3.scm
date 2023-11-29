#lang racket

(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch p m)
    (if (eq? p password) (cond ((eq? m 'withdraw) withdraw)
                               ((eq? m 'deposit) deposit)
                               (else (error "Unknown request -- MAKE ACCOUNT" m)))
        (lambda (m) (display "Incorrect password"))))

  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 50)

#|
We had to use `(lambda (m) (display "Incorrect password"))` because `dispatch` is supposed to return a procedure.
If we just used `(display "Incorrect password")` it will return a string which doesn't work.
|#