#lang racket

; We need to attach type-tags of which "division" to files and records

(define (get-record name file)
  (let ( (proc (get 'get-record (type-tag file))) )
    (if proc (proc name)
        false)))

; get-salary is just the same
(define (get-salary record)
  (let ( (proc (get 'get-salary (type-tag record))) )
    (if proc (proc record)
        false)))

; search for all records in all files
; lof is list of files
(define (find-employee-record name lof)
  (if (null? lof)
      (error "Employee not found")
      (append (get-record name (car lof))
              (find-employee-record name (cdr lof)))))


  