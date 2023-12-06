#lang racket
(require r5rs)
(require racket/trace)

(define (make-table) (list '*table*))

(define (lookup keylist table) ; a table is also a record
  (cond ((not table) false)
        ((null? keylist) (cdr table))
        (else (lookup (cdr keylist) (assoc (car keylist) (cdr table))))))


(define (insert! keylist value table)
  (if (null? keylist) (set-cdr! table value)
      (let ((subtable (assoc (car keylist) (cdr table))))
        (if subtable
            (insert! (cdr keylist) value subtable) ; recursively call insert for next keys and subtables
            (begin ; if not subtable
              (set-cdr! table
                        (cons (list (car keylist)) (cdr table)))
              ; The new (k v) pair will be an alist / subtable whose key is the current key `(list (car keylist))`
              ;its value still not computed (the cdr of the list)
              ; we attach it to the beginning of the current table using cons
              (insert! (cdr keylist) value (cadr table)))
            ; now the table is updated and its first entry is our new key
            ;now recursively call the insertion on this first entry (alist / subtable)
            ))))
  
        

(define t (make-table))
;(insert! (list 'a) 1 t)
;(insert! (list 'a 'b) 2 t)
;(insert! (list 'a 'b 'c) 3 t)
t
;(lookup (list 'a 'b) t)

; This solution assumes all entries are compatible
; it works to insert arbitrary number of keys but should be consistent in any table
#|
That solution assumes all the entries are compatible.  If you say
	(insert! '(a) 'a-value my-table)
	(insert! '(a b) 'ab-value my-table)
the second call will fail because it will try to
	(assoc 'b (cdr 'a-value))
and the CDR will cause an error.
|#