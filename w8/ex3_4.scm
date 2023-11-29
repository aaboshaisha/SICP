#lang racket

(define (make-account balance password)
  (define access-count 0) ; added this line
  (define (call-the-cops m) ; also added this procedure (we could have put it inside the else of dispatch
      (if (< access-count 7)
          (begin
            (set! access-count (+ 1 access-count))
            (display "Incorrect password")
            (newline)
            )
          (begin
          (display "Call the Cops")
          (newline))
          )
      )
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch p m)
    (if (eq? p password)
        (begin 
        (set! access-count 0) ; RESET when correct password used
        (cond ((eq? m 'withdraw) withdraw)
                               ((eq? m 'deposit) deposit)
                               (else (error "Unknown request -- MAKE ACCOUNT" m)))
        )
        call-the-cops
        ))
  
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 50)


(define (test n)
  (if (> n 0)
      (begin 
        ((acc 'some-other-password 'withdraw) 50)
        (test (- n 1)))
      'done
      ))
(test 8)

((acc 'secret-password 'withdraw) 10)

