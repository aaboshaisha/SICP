#lang racket
(require berkeley)
(require racket/trace)


(define (map-1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map-1 proc (cdr items)))))

;(map-1 square (list 1 2 3 4))


(define (substitute1 items old new)
  (map-1 (lambda (item) (if (equal? item old) new item)) items))

(define (substitute3 items old new)
  (if (null? items)
      nil
      (if (word? (car items))
          (map (lambda (item) (if (equal? item old) new item)) items)
          (cons (substitute3 (car items) old new) (substitute3 (cdr items) old new)))))


#|Write a procedure substitute that takes three arguments: a list, an old word, and a new word.
It should return a copy of the list, but with every occurrence of the old word replaced by the new word,
even in sublists. |#

(define (substitute items old new)
  (cond ((null? items) nil)
        ((word? items) (if (equal? items old) new items))
        (else (cons (substitute (car items) old new) (substitute (cdr items) old new)))))

;(trace substitute)
(substitute '(lead guitar) 'guitar 'axe)
(substitute '((lead guitar) (bass guitar)) 'guitar 'axe)
(substitute 'guitar 'guitar 'axe)
(substitute '((lead guitar) (bass guitar) 'guitar 'bass) 'guitar 'axe)
(substitute '((lead guitar) (bass guitar) (rhythm guitar) drums) 'guitar 'axe)


; substitute2
#|Now write substitute2 that takes a list, a list of old words, and a list of new words;
the last two lists should be the same length. It should return a copy of the first argument,
but with each word that occurs in the second argument replaced by the corresponding word of
the third argument |#

; (map proc-n-args n-lists) -> applies proc to all 1st elements of lists, all 2nd elements etc
(map +
     (list 1 2 3)
     (list 10 20 30)
     (list 100 200 300))


(map (lambda (x y) y)
     '(1 2 3 4)
     '(one two three four))

(define (match-and-replace word list-1 list-2)
  (cond ((null? list-1) word)
        ((equal? word (car list-1)) (car list-2))
        (else (match-and-replace word (cdr list-1) (cdr list-2)))))

(match-and-replace '5 '(1 2 3 4) '(one two three four))
(match-and-replace '4 '(1 2 3 4) '(one two three four))


(define (substitute2 items old-words new-words)
  (cond ((null? items) nil)
        ((word? items) (match-and-replace items old-words new-words))
        (else (cons (substitute2 (car items) old-words new-words)
                    (substitute2 (cdr items) old-words new-words)))
        ))

;testing
;(trace substitute2)
;(trace match-and-replace)
(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
                                 '(1 2 3 4)
                                 '(one two three four))

(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves) '1 '2)
                                 '(1 2 3 4)
                                 '(one two three four))