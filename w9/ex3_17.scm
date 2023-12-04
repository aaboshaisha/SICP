#lang racket
(require racket/trace)

(define (find-pair x items) ;to look for item in list
  (cond ((null? items) #f)
        ((equal? x (car items)) #t)
        (else (find-pair x (cdr items)))))


#|
(define counted-pairs (list ))
(define (count-pairs x)
  (if (not (pair? x))
      0
      (if (not (find-pair x counted-pairs))
          (begin
            (set! counted-pairs (cons x counted-pairs))
            (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)
            )
            (+ (count-pairs (car x)) (count-pairs (cdr x))))))
|#
; We want to do this but keeping the state local


#|
(define (count-pairs x)
  (define counted-pairs (list )) ;save the counted-pairs

  (define (count x)
    (if (not (pair? x))
        0
        (if (not (find-pair x counted-pairs))
            (begin
              (set! counted-pairs (cons x counted-pairs))
              (+ (count (car x)) (count (cdr x)) 1)
              )
            (+ (count (car x)) (count (cdr x))))))
  
  (count x))
|#

; A similar version makes use of let to create the local state
; it also removed the duplicate code (+ (count (car x)) (count (cdr x))) which is same as returning 0
; it uses built-in memq instead of our custom-defined `find-pair`

#|
(define (count-pairs x)
  (let ((counted-pairs '())) ;save the counted-pairs outside the procedure but let the procedure have access to it
    (define (count x)
      (if (or (not (pair? x)) (memq x counted-pairs))
          0
          (begin
            (set! counted-pairs (cons x counted-pairs))
            (+ (count (car x)) (count (cdr x)) 1))
          )
      )
    (count x)))
|#

; One more way to do this
(define (count-pairs lst)
  (let ((counted-pairs '())
        (count 0))

    (define (count-and-add-pair pair) ; counts a pair and adds it to counted list
      (set! counted-pairs (cons pair counted-pairs))
      (set! count (+ count 1)))

    (define (recur pair) ; recursively traverses the pair and decides whether to count or not
      (cond ((not (pair? pair)) '())
            ((memq pair counted-pairs) '())
            (else (count-and-add-pair pair)
                  (recur (car pair))
                  (recur (cdr pair)))))
    (recur lst)
    count))

    

(count-pairs (list 1 2 3))
(count-pairs (cons 1 (cons 2 (cons 3 '()))))

(define x '(foo))
(define y (cons x x))
(define z (list y))
(count-pairs z)

(define h (list 1))
(define i (cons h h))
(define j (cons i i))
(count-pairs j)
