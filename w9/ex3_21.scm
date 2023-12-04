#lang racket
(require r5rs)

; a queue is a pair of pointers to front and rear
(define (make-queue) (cons '() '()))
; selectors to access front and rear
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
; to modify front and rear
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
; is it empty
(define (empty-queue? queue) (null? (front-ptr queue)))
; select the item at the front of the queue
(define (front-queue queue)
  (if (empty-queue? queue) (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
;insert into back
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair) ; chain the last item to the new one
           (set-rear-ptr! queue new-pair) ; make the rear ptr to the new item
           queue))))

; delete from front of queue
(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue))) ; make the front ptr to the 2nd item
         queue)))


(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)


(define (print-queue queue) (front-ptr queue))

(display "Using print-queue")
(newline)
(define q2 (make-queue))
(print-queue q2)
(insert-queue! q2 'a)
(print-queue q2)
(insert-queue! q2 'b)
(print-queue q2)
(delete-queue! q2)
(print-queue q2)
(delete-queue! q2)
(print-queue q2)
