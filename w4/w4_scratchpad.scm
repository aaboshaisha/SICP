#lang racket
(require berkeley)

(cons 1 (cons 2 (cons 3 (cons 4 nil))))
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))

(car  one-through-four)
(cdr  one-through-four)
(cadr one-through-four)

(cons 5 one-through-four)
(cons one-through-four 5)

; cdr-ing down a list a.k.a list indexing
(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(display "Testing list indexing")
(newline)
(list-ref one-through-four 0)
(list-ref one-through-four 1)
(list-ref one-through-four 2)
(list-ref one-through-four 3)


(define (length lst)
  (if (empty? lst)
      0
      (+ 1 (length (cdr lst)))))

(display "Test len(list)")
(newline)
(length one-through-four)

; appending two lists
#| We've seen we could only append 1 item to beginng of list using cons
We'll therefore need to append items from list1 to front of list2 one by one using cons till
list1 is empty
|#

(define (append list1 list2)
  (if (empty? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define odds (list 1 3 5 7))
(define evens (list 2 4 6 8))

(append odds evens)
(append evens odds)

