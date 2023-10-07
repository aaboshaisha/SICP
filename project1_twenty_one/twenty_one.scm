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

;Testing
;(dealer-sensitive '(Ad 5s) 'Ah)
;(dealer-sensitive '(Ad 2s) '7h)
;(dealer-sensitive '(Kt 9s) 'Ah)


; Generalize stop-at-n
;(stop-at n) should return a strategy that keeps hitting until a hand’s total is n or more
(define (stop-at-n n)
  (lambda (customer-hand-so-far dealer-up-card)
    (if (< (best-total customer-hand-so-far) n) #t #f)))

;(stop-at-n 17)
;((stop-at-n 17) '(Ad 5s 5h) (word 'as))
; test if strategy works in a game
;(twenty-one (stop-at-n 12))
;(play-n (stop-at-n 12) 20)

; Valentine
(define (heart-in-hand customer-hand-so-far)
  (if (empty? customer-hand-so-far)
      #f
      (or (equal? (last (first customer-hand-so-far)) 'H)
          (heart-in-hand (butfirst customer-hand-so-far)) )
      ))

;(heart-in-hand '(AS AH 3T))
;(heart-in-hand '(3S 4D 10C))

(define (valentine customer-hand-so-far dealer-up-card)
  (if (heart-in-hand customer-hand-so-far)
      ((stop-at-n 19) customer-hand-so-far dealer-up-card)
      ((stop-at-n 17) customer-hand-so-far dealer-up-card)))

;Suits: SHDC
;(trace valentine)


;(play-n valentine 20)

; suit-strategy
(define (suit-in-hand hand suit)
  (if (empty? hand)
      #f
      (or (equal? (last (first hand)) suit) (suit-in-hand (butfirst hand) suit) )))

;(suit-in-hand '(AS AC 5H) 'S)
;(suit-in-hand '(AS AC 5H) 'T)

(define (suit-strategy suit strategy-not strategy-inc)
  (lambda (customer-hand dealer-card)
    (if (suit-in-hand customer-hand suit)
        strategy-inc
        strategy-not)))

(define (valentine2 customer-hand-so-far dealer-up-card)
  (((suit-strategy 'H (stop-at-n 17) (stop-at-n 19)) customer-hand-so-far dealer-up-card)
  customer-hand-so-far dealer-up-card))

;(trace suit-strategy)
;'(Testing valentine against valentine2)
;(valentine '(KC 9S) 'AD)
;(valentine '(KH 2S) '3D)
;(valentine2 '(KC 9S) 'AD)
;(valentine2 '(KH 2S) '3D)

; majority
;Define a function majority that takes three strategies as arguments and produces a strategy
;as a result, such that the result strategy always decides whether or not to “hit” by consulting
;the three argument strategies, and going with the majority.
;That is, the result strategy should return #t if and only if at least two of the three
;argument strategies do.

(define (majority s1 s2 s3)
  (lambda (hand card)
    (cond ((and (s1 hand card) (s2 hand card)) #t)
          ((and (s1 hand card) (s3 hand card)) #t)
          ((and (s2 hand card) (s3 hand card)) #t)
          ((and (s1 hand card) (s2 hand card) (s3 hand card)) #t)
          (else #f))))

((majority stop-at-17 dealer-sensitive valentine) '(KC 9S) 'AD)
          
(play-n (majority stop-at-17 dealer-sensitive valentine) 50)



 