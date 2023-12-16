#lang racket
(require berkeley)

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(stream-ref y 7)

(display-stream z)

#|

(define sum 0) : here sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20))) :
- (stream-enumerate-interval 1 20): gives seq -> (1 . #promise)
- 1 is available for (accum ) so sum = 1


(define y (stream-filter even? seq)) :
seq -> (1 . #promise)
stream-filter keeps forcing the cdr of the stream till it can extract a stream-car that is even
seq -> (1 3 6 . promise) since interval -> 1+2+3 = 6 (even)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq)) :
similar thing happens till we get 10 (seq 1 3 6 10)

(stream-ref y 7) -> this starts to compute the elements of seq till y 7
starts at y (6 10 28 36 . promise)
we get (y 7) when seq is at (1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136)
(this is interval at 16)
where y -> (6 10 28 36 66 78 120 136)
(stream-ref y 7) now prints 136

| stream-ref | interval | seq                        | sum                | y  | z  |
|------------|----------|----------------------------|--------------------|----|----|
| 0          | 1        | (1 . promise)              | 1                  | -  |    |
| 1          | 2        | (1 3 . promise)            | 1 + 2 = 3          | -  |    |
| 2          | 3        | (1 3 6 . promise)           | 3 + 3 = 6          | 6  |    |
| 3          | 4        | (1 3 6 10 . promise)        | 6 + 4 = 10         | 10  | 10 |
| 4          | 5        | (1 3 6 10 15 . promise)     | 10 + 5 = 15        | -  | 15 |
| 5          | 6        | (1 3 6 10 15 21 . promise)  | 15 + 6 = 21        | -  |    |
| 6          | 7        | (1 3 6 10 15 21 28 . promise)| 21 + 7 = 28        | 28  |    |
| 7          | 8        | (1 3 6 10 15 21 28 36 . promise)| 28 + 8 = 36      | 36  |    |
	  

(display-stream z):
Now z (10 15 45 55 105 120)
It keeps forcing the interval (17 18 19 20)
17
18
19 -> sum prints 190
20 -> sum prints 210

If we didn't use memoization: the intermediate results will have to be printed from the beginning each time.
Also, we get the wrong result since `sum` is defined as a global variable that will be modified by each of these
procedures each time.

|#