#lang racket
(require berkeley)

; my code here
(define (best-total hand)
  ; score cards as if ace = 11
  (define (score-card card)
    (cond ((equal? (butlast card) 'A) 11)
          ((or (equal? (butlast card) 'J) (equal? (butlast card) 'Q) (equal? (butlast card) 'K)) 10)
          (else (butlast card))))

  ; count how many aces in a hand
  (define (count-aces hand)
    (define (ace? c)
      (if (equal? (butlast c) 'A) 1 0))
    (if (empty? hand)
        0
        (+ (ace? (first hand)) (count-aces (butfirst hand)))))

  ; compute total with ace = 11
  (define (total hand)
    (if (empty? hand)
        0
        (+ (score-card (first hand)) (total (butfirst hand)))))

  ; If our hand has one or more aces, we can change each one
  ; Keep changing ace from 11 to 1 till total < 21 or u run out of aces to change
  (define (update hand-total ace-count)
    (if (and (> hand-total 21) (> ace-count 0))
        (update (- hand-total 10) (- ace-count 1))
        hand-total))
  (update (total hand) (count-aces hand)))

;testing
;(best-total '(Ad 8s))
;(best-total '(Ad 8s 5h))
;(best-total '(Ad As 9h))


; Define a strategy procedure stop-at-17 that’s identical to the dealer’s, i.e., takes a
; card if and only if the total so far is less than 17.
;strategy function should return a true or false output,
;which tells whether or not the customer wants another card

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  (if (< (best-total customer-hand-so-far) 17) #t #f))

;testing
;(stop-at-17 '(Ad 5h) (word 'as))
;(stop-at-17 '(Ad 5s 5h) (word 'as))

;----------------------------------------------------------------------------------
; starter code
(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )


; play-n
(define (play-n strategy n)
  (if (= n 0)
      0
      (+ (twenty-one strategy) (play-n strategy (- n 1)))))


(require racket/trace)
;(trace stop-at-17)
;(play-n stop-at-17 1)
;(play-n stop-at-17 3)
;(play-n stop-at-17 10)

; dealer sensitive
(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  (if (or (and (or (equal? (butlast dealer-up-card) 7)
                   (equal? (butlast dealer-up-card) 8)
                   (equal? (butlast dealer-up-card) 9)
                   (equal? (butlast dealer-up-card) 10))
                   (equal? (butlast dealer-up-card) 'A)
                   (equal? (butlast dealer-up-card) 'J) 
                   (equal? (butlast dealer-up-card) 'Q)
                   (equal? (butlast dealer-up-card) 'K))
               
               (< (best-total customer-hand-so-far) 17)
          
          (and (or (equal? (butlast dealer-up-card) 2)
                   (equal? (butlast dealer-up-card) 3)
                   (equal? (butlast dealer-up-card) 4)
                   (equal? (butlast dealer-up-card) 5)
                   (equal? (butlast dealer-up-card) 6))
               (< (best-total customer-hand-so-far) 12)))
      #t
      #f))



;(trace dealer-sensitive)

;(best-total '(Ad 8s 5h))
;(best-total '(Ad As 9h))
(dealer-sensitive '(Ad 5s) 'Ah)
(dealer-sensitive '(Ad 2s) '7h)
(dealer-sensitive '(Kt 9s) 'Ah)
