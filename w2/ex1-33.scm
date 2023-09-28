#lang racket
(require berkeley)

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null-value
                    (filtered-accumulate combiner null-value term (next a) next b filter))
          )))
          


(define (identity x) x)
(define (inc x) (+ 1 x))


(define (sum a b)
  (filtered-accumulate + 0 identity a inc b identity))

(define (product a b)
  (filtered-accumulate * 1 identity a inc b identity))

(define (sum-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-relative-primes a b)

  (define (relatively-prime a)
    (if (and (< a b) (= 1 (gcd a b)))
        #t
        #f))
  
  (filtered-accumulate * 1 identity a inc b relatively-prime))


(product-relative-primes 1 10) ;result 189



