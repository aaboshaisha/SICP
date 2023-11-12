#lang racket



(define (treemap-1 f tree)
  (cons (f (car tree))
        (map (lambda (t) (treemap-1 f t)) (cdr tree))))

(define (treemap-2 f tree)
  (cons (f (car tree))
        (forest-map f (cdr tree))))

(define (forest-map f items)
  (if (null? items)
      '()
      (cons (treemap-2 f (car items)) (forest-map f (cdr items)))))

(define t (cons 7 (list (cons 2 (cons 1 3)) (cons 5 (cons 4 6)))))
(define (leaves . seq)
  (map (lambda (x) (cons x '())) seq))
(define t1 (cons 7
                      (list (cons 2 (leaves 1 3))
                            (cons 5 (leaves 4 6)))))

(define (deepmap f lol)
  (cond ((null? lol) '())
        ((not (pair? lol)) (f lol))
        (else (cons (deepmap f (car lol))
                    (deepmap f (cdr lol))))))


(define (deepmap-2 f lol)
  (if (not (pair? lol))
      (f lol)
      (map (lambda (element) (deepmap-2 f element)) lol)))


(define lol
  '((1 2 3)
    (4 5 6)
    (7 8 9)))


#|
(define (deepmap-2 fn lol)
(if (list? lol)
(map (lambda (element) (deepmap-2 fn element))
           lol)
      (fn lol)))
|#
(define dl (list (list 1 2) (list 3 4) (list 5 6)))

(define (square x) (* x x))




;(treemap-1 square t)
;(treemap-2 square t)
;(treemap-1 square t1)
;(treemap-2 square t1)
;(treemap-2 square dl)
;(deepmap square dl)
;(deepmap square t)
;(deepmap square t1)
;(deepmap square lol)

(deepmap-2 square dl)
;(deepmap-2 square t)
(deepmap-2 square t1)
(deepmap-2 square lol)

(define (filter pred seq)
  (cond ((null? seq) '())
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq))))) 


(define (for-each1 f items)
  (cond ((null? (cdr items)) (f (car items))) ; one element list
        (else (f (car items)) (for-each1 f (cdr items))) ))



(for-each1 square (list 1 2 3 4))
      