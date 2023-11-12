#lang racket

(define the-get/put-table (make-hash))

(define (get key1 key2)
  (hash-ref the-get/put-table (list key1 key2) #f))

(define (put key1 key2 value)
  (hash-set! the-get/put-table (list key1 key2) value)
  'ok)


; We want to be able to say, e.g., (area circle3) to get the area of a particular (previously defined) circle.

; our constructors and selectors
(define (attach-tag tag datum) (cons tag datum))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

; A circle is defined by its radius r
; A square is define by its side s

(define (make-square side)
  (attach-tag 'square side))

(define (make-circle radius)
  (attach-tag 'circle radius))


(define s5 (make-square 5))
(define c3 (make-circle 3))


(put 'square 'area (lambda (s) (* s s)))
(put 'square 'perimeter (lambda (s) (* 4 s)))
(put 'circle 'area (lambda (r) (* pi r r)))
(put 'circle 'perimeter (lambda (r) (* 2 pi r)))


#|
(define (operate op obj)
  (let ( (type (type-tag obj))
         (parameter (contents obj)) )
    (if (get type op)
        (let ( (proc (get type op)) )
          (proc parameter))
        (error "No procedure for such type --"))))
|#

(define (operate op obj)
  (let ( (proc (get (type-tag obj) op)) )
    (if proc
        (proc (contents obj))
        (error "Unkonow operator for type"))))



(define (area shape) (operate 'area shape))
(define (perimeter shape) (operate 'perimeter shape))



(area s5)
(area c3)
(perimeter s5)
(perimeter c3)

