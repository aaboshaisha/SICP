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
        (lambda (m)
          (begin
          (display "Incorrect password")
          (newline))
          )))

  dispatch)




#|
(define (make-joint account account-password new-password)
  (lambda (p m)
    (if (eq? p new-password)
        (lambda (amount) ((account account-password m) amount))
        (lambda (amount) ((account new-password m) amount))
        )))
|#


#|
We know account (object) is a dispatch procedure.
We can make the joint account another procedure that checks is the password is right
and since it has acess to the original account's password, it can call it with the old password
|#
; An even simpler version
(define (make-joint account account-password new-password)
  (lambda (p m)
    (if (eq? p new-password)
        (account account-password m)
        (lambda (x) "Incorrect password")
        )))


(define acc (make-account 100 'secret-password))
(define paul-acc
  (make-joint acc 'secret-password 'new-password))

((paul-acc 'new-password 'withdraw) 50)
((paul-acc 'secret-password 'withdraw) 50)
((acc 'secret-password 'withdraw) 10)
((acc 'some-other-password 'withdraw) 50)