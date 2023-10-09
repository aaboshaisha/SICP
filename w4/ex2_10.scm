#lang racket
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (zero-division a b)
  (if (= b 0)
      (begin (display "ZeroDivisionError") (newline))
      (/ a b)))

(define (div-interval x y)
  (let (
        (a (lower-bound y))
        (b (upper-bound y))
        )
    (if (and (<= a 0) (>= b 0))
        (begin (display "Error: Interval spans 0") (newline))
        ;(error "Error: Interval spans 0")
        (mul-interval x 
                (make-interval 
                 (zero-division 1.0 (upper-bound y)) 
                 (zero-division 1.0 (lower-bound y)))))))

;(zero-division 1 0)
;(zero-division 1 2)



  

(define x (make-interval 2 10))
(define y (make-interval -3 3))
(define y1 (make-interval 0 3))
(define y2 (make-interval -3 0))
(define y3 (make-interval -3 -1))
(define y4 (make-interval 1 3))


(div-interval x y)
(div-interval y x)
(div-interval x y1)
(div-interval x y2)
(div-interval x y3)
(div-interval x y4)