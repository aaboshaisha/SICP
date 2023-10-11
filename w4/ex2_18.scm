#lang racket
(require berkeley)


(require racket/trace)

(define (reverse lst)
  (if (empty? lst)
      nil
      (append (reverse (cdr lst)) (list (car lst)))))

;(trace reverse)
;(reverse (list 1 2 3 4))
(define (reverse1 lst)
  (define (iter lst item)
    (if (empty? lst)
        item
        (iter (cdr lst) (cons (car lst) item))))
  (iter lst nil))

(reverse1 (list 1 2 3 4))