#lang racket
; This is very easy

(define S (cons-stream 1 (merge (scale-stream S 2)
				  (merge (scale-stream S 3)
					 (scale-stream S 5)))))