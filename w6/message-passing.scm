#lang racket

(define (make-square side)
  (lambda (message)
    (cond ((eq? message 'area) (* side side))
          ((eq? message 'perimeter) (* 4 side))
          (else (error "Unknown message")))))

(define (make-circle radius)
  (lambda (message)
    (cond ((eq? message 'area) (* pi radius radius))
          ((eq? message 'perimeter) (* 2 pi radius))
          (else (error "Unkown message")))))

(define c3 (make-circle 3))
(define s4 (make-square 4))

(define (operate op obj)
  (obj op))

(define (area shape) (operate 'area shape))
(define (perimeter shape) (operate 'perimeter shape))

(area c3)
(perimeter c3)
(area s4)
(perimeter s4)
                                    
                         