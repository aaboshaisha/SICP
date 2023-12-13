#lang racket

(define (make-account-and-serializer balance)
  (let ((next-account-id 1) ; accounts start with 1 (global) 
        (id-serializer (make-serializer))) ; since account number update is via access (see below)
    (lambda (balance)
      (define (withdraw amount)
        (if (>= balance amount)
            (begin 
              (set! balance (- balance amount)) balance)
            "Insufficient funds"))

      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)

      ((id-serializer ; protect id access 
        (let ((balance-serializer (make-serializer))
              ((ID next-account-id)))
          (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'serializer) balance-serializer)
                  ((eq? m 'ID) ID)
                  (else (error "Unknown request: MAKE-ACCOUNT" m))))
          (set! next-id-number (+ next-id-number 1)) ; and incremented here when the dispatch (object) is created
          dispatch))))))



(define (serialized-exchange account1 account2)
  (if (< (account1 'ID) (account2 'ID))
      (let ((serializer1 (account1 'serializer))
            (serializer2 (account2 'serializer)))
        ((serializer1 (serializer2 exchange)) account1 account2))

      (let ((serializer2 (account1 'serializer))
            (serializer1 (account2 'serializer)))
        ((serializer1 (serializer2 exchange)) account1 account2))
      ))