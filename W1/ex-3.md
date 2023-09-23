Write a procedure switch that takes a sentence as its argument and returns a sentence in which every instance of the words I or me is replaced by you,
while every instance of you is replaced by me except at the beginning of th sentence, where it’s replaced by I. (Don’t worry about capitalization of letters.)

Example:
```
> (switch ’(You told me that I should wake you up)) 
> (i told you that you should wake me up)
```

-----------------
How to solve this
-----------------
At a first glance, there seems to be many parts to this problem. We need to detect I, you, me in sentences and replace each. We also need to decide whether you comes in the beginning of sentence or not such that we replace it accordingly. 

Our first step is to think of a simpler problem or part of the problem. We could just think of a procedure that replaces each I or me with you and each you with me. This could be done with a simple cond():

```
  (cond ((equal? w 'I) 'you)
        ((equal? w 'me) 'you)
        ((equal? w 'you) 'me)
```

and we might notice that if 'you' comes at the beginning of a sentence, then (assuming proper capitalization) it must be 'You' so we add that condition:

```
(define (replace w)
  (cond ((equal? w 'I) 'you)
        ((equal? w 'me) 'you)
        ((equal? w 'you) 'me)
        ((equal? w 'You) 'I)
        (else w))
  )
```
As for iterating through the sentence, this cane be done by applying a function `switch` to the beginning of each sentence then use `butfirst` to progressively move the beginning of the sentence 

```
(define (switch s)
  (if (empty? s)
      '()
      (se (replace (first s))
          (switch (butfirst s)))
      )
  )

```


To do a more roubst version that takes into account the user may type things without caring about capitalization, we can add another procedure that ensures that if 'you' is at the beginning of a sentence, we capitalize it then do the switching

```
#lang racket
(require berkeley)

(define (replace w)
  (cond ((equal? w 'I) 'you)
        ((equal? w 'i) 'you)
        ((equal? w 'me) 'you)
        ((equal? w 'you) 'me)
        ((equal? w 'You) 'I)
        (else w))
  )


(define (cap-first s)
  (if (equal? (first s) 'you) (se 'You (butfirst s))
      s))


(define (cswitch s)
  (if (empty? s)
      '()
      (se (replace (first s))
          (cswitch (butfirst s)))
      )
  )


(define (switch s)
  (cswitch (cap-first s)))
```
