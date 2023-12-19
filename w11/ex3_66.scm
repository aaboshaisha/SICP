#lang racket
(require berkeley)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (print-stream-up-to-n stream n)
  (let loop ((s stream) (count 0))
    (cond ((and (not (stream-null? s)) (< count n))
           (display (stream-car s))
           (display " ")
           (loop (stream-cdr s) (+ count 1)))
          (else 'done))))

(define s (pairs integers integers))

;(print-stream-up-to-n s 40)

(define (count-pairs-before-p stream p count)
  (if (equal? (stream-car stream) p)
      count
      (count-pairs-before-p (stream-cdr stream) p (+ count 1))))

; the first element in each row (diagonals)
(count-pairs-before-p s '(1 1) 0)
(count-pairs-before-p s '(2 2) 0)
(count-pairs-before-p s '(3 3) 0)
(count-pairs-before-p s '(4 4) 0)
(count-pairs-before-p s '(5 5) 0)
(count-pairs-before-p s '(6 6) 0)
(count-pairs-before-p s '(7 7) 0)
(count-pairs-before-p s '(8 8) 0)
(count-pairs-before-p s '(9 9) 0)
(count-pairs-before-p s '(10 10) 0)

; the rest of the elements (up to n) in the first row
(count-pairs-before-p s '(1 1) 0)
(count-pairs-before-p s '(1 2) 0)
(count-pairs-before-p s '(1 3) 0)
(count-pairs-before-p s '(1 4) 0)
(count-pairs-before-p s '(1 5) 0)
(count-pairs-before-p s '(1 6) 0)
(count-pairs-before-p s '(1 7) 0)
(count-pairs-before-p s '(1 8) 0)
(count-pairs-before-p s '(1 9) 0)
(count-pairs-before-p s '(1 10) 0)



(count-pairs-before-p s '(2 2) 0)
(count-pairs-before-p s '(2 3) 0)
(count-pairs-before-p s '(2 4) 0)
(count-pairs-before-p s '(2 5) 0)
(count-pairs-before-p s '(2 6) 0)
(count-pairs-before-p s '(2 7) 0)
(count-pairs-before-p s '(2 8) 0)
(count-pairs-before-p s '(2 9) 0)
(count-pairs-before-p s '(2 10) 0)
(count-pairs-before-p s '(2 11) 0)


(display "for 3")
(newline)
(count-pairs-before-p s '(3 3) 0)
(count-pairs-before-p s '(3 4) 0)
(count-pairs-before-p s '(3 5) 0)
(count-pairs-before-p s '(3 6) 0)
(count-pairs-before-p s '(3 7) 0)
(count-pairs-before-p s '(3 8) 0)
(count-pairs-before-p s '(3 9) 0)
(count-pairs-before-p s '(3 10) 0)
(count-pairs-before-p s '(3 11) 0)
(count-pairs-before-p s '(3 12) 0)


#|
The general stream of pairs (pairs S T) is composed of 3 parts:
- the pair (s0 t0)
- the rest of the pairs in its row
- the remaining pairs

If we focus on the diagonal elements (which represent the pair as in (s0 t0) (s1 t1) and so on), we see the
following pattern

| Function Call                           | Result | Formula          |
|-----------------------------------------|--------|-------------------|
| (count-pairs-before-p s '(1 1) 0)       | 0      | 2^1 - 2           |
| (count-pairs-before-p s '(2 2) 0)       | 2      | 2^2 - 2           |
| (count-pairs-before-p s '(3 3) 0)       | 6      | 2^3 - 2           |
| (count-pairs-before-p s '(4 4) 0)       | 14     |                   |
| (count-pairs-before-p s '(5 5) 0)       | 30     |                   |
| (count-pairs-before-p s '(6 6) 0)       | 62     |                   |
| (count-pairs-before-p s '(7 7) 0)       | 126    |                   |
| (count-pairs-before-p s '(8 8) 0)       | 254    |                   |
| (count-pairs-before-p s '(9 9) 0)       | 510    |                   |
| (count-pairs-before-p s '(10 10) 0)     | 1022   | 2^10 - 2          |

If our pairs are (i j)
i = i : 2^i - 2 

j > i : (2^i - 2) + ( (j - i) * 2^i) - 2

|#

