#lang racket
(require berkeley)

; my code here
(define (best-total hand)
  ; score cards as if ace = 11
  (define (score-card card)
    (cond ((equal? (butlast card) 'a) 11)
          ((or (equal? (butlast card) 'j) (equal? (butlast card) 'q) (equal? (butlast card) 'k)) 10)
          (else (butlast card))))

  ; count how many aces in a hand
  (define (count-aces hand)
    (define (ace? c)
      (if (equal? (butlast c) 'a) 1 0))
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

(best-total '(ad 8s))
(best-total '(ad 8s 5h))
(best-total '(ad as 9h))


; Define a strategy procedure stop-at-17 that’s identical to the dealer’s, i.e., takes a
; card if and only if the total so far is less than 17.
;strategy function should return a true or false output,
;which tells whether or not the customer wants another card

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  (if (< (best-total customer-hand-so-far) 17) #t #f))

;testing
(stop-at-17 '(ad 5h) (word 'as))
(stop-at-17 '(ad 5s 5h) (word 'as))

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















;                                      32
