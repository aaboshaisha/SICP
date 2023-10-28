#lang racket
(require berkeley)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

;(enumerate-interval 1 5)

#|
for-each x procedures are written as (map .... to x)
for-each i : 1 -> n - 1
   generate j: i + 1 -> n - 1
      generate k: j + 1 -> n and make (i, j , k)

so we start writing this:

generate i interval using map

(map (lambda (i) xxx ) (enumerate-interval 1 (- n 1)))

i will be used inside a machine that generates j :

(map (lambda (j) xxx ) (enumerate-interval (+ i 1) (- n 1)))

Which will also need a machine to generate k and gives list when u have i, j and k

(map (lambda (k) (list i j k)) (enumerate-interval (+ j 1) n))


|#

; generate all possible unique ordered triplets 
(define (unique-triplets n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j)
                                      (map (lambda (k) (list i j k))
                                           (enumerate-interval (+ j 1) n)))
                                    (enumerate-interval (+ i 1) (- n 1))))
                   (enumerate-interval 1 (- n 1)))
              ))

;(unique-triplets 4)

; this needs to be flattened to a list of lists
(define (flatten-triplets-list items)
  (if (null? items)
      nil
      (append (map car items) (flatten-triplets-list (cdr items)))))

;(flatten-triplets-list (unique-triplets 4))

; filter items that sum to "s"

(define (filter predicate? items)
  (cond ((null? items) nil)
        ((predicate? (car items)) (cons (car items) (filter predicate? (cdr items))))
        (else (filter predicate? (cdr items)))))

;(filter odd? (list 1 2 3 4 5))


#|
(define (triplet-sums-to-s t s)
  (let ( (i (car t))
         (j (car (cdr t)))
         (k (car (cdr (cdr t)))))
    (if (= s (+ i j k))
        #t
        #f
        )))

;(triplet-sums-to-s (list 1 2 3) 6)
;(triplet-sums-to-s (list 1 2 3) 9)

|#

(define (triplet-sums-to-s? s)
  (lambda (t)
    (let ( (i (car t))
         (j (car (cdr t)))
         (k (car (cdr (cdr t)))))
    (if (= s (+ i j k))
        #t
        #f
        ))))

;((triplet-sums-to-s? 6) (list 1 2 3))


(define (find-ordered-triplets n s)
  (filter (triplet-sums-to-s? s) (flatten-triplets-list (unique-triplets n))))

;(find-ordered-triplets 4 6)
;(find-ordered-triplets 20 30)

;(flatten-triplets-list (unique-triplets 4))
