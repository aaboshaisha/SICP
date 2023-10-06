#lang racket
(require berkeley)

;Define a procedure best-total that takes a hand (a sentence of card words) as argument,
;and returns the total number of points in the hand.
;It’s called best-total because if a hand contains aces,
;it may have several different totals.
;The procedure should return the largest possible total that’s less than or equal to 21, if possible.


; First: we need to be able to score any card. We'll start with all aces = 11
(define (score-card card)
  (cond ((equal? (butlast card) 'a) 11)
        ((or (equal? (butlast card) 'j) (equal? (butlast card) 'q) (equal? (butlast card) 'k)) 10)
        (else (butlast card))))

(score-card '10s)
(score-card 'as)
(score-card 'ad)

; To score an ace with 1 or 11 will depend on total score and how many aces are there we could change
; We want to count how many aces are in a hand
(define (ace? c)
  (if (equal? (butlast c) 'a) 1
      0))


(ace? 'ad)
(ace? '8j)

(define (count-aces hand)
  (if (empty? hand)
      0
      (+ (ace? (first hand)) (count-aces (butfirst hand)))))

(count-aces '(ad 8s))
(count-aces '(ad 8s 5h))
(count-aces '(ad as 9h))
(count-aces '(5h 8s))

; Compute a total that assumes ace = 11
(define (total hand)
  (if (empty? hand)
      0
      (+ (score-card (first hand)) (total (butfirst hand)))))

(total '(ad 8s))
(total '(ad 8s 5h))
(total '(ad as 9h))
(total '(5h 8s))

; If our hand has 0 aces, we can't change anything
; If our hand has one or more aces, we can change each one
; Keep changing ace from 11 to 1 till total < 21 or u run out of aces to change
; then no more changes needed
(define (best-total hand)
  (define (update hand-total ace-count)
    (if (and (> hand-total 21) (> ace-count 0))
        (update (- hand-total 10) (- ace-count 1))
        hand-total))
  (update (total hand) (count-aces hand)))

(best-total '(ad 8s))
(best-total '(ad 8s 5h))
(best-total '(ad as 9h))