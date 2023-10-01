# Week2 - Higher order procedures

**abstraction**: giving names to common patterns and working with the abstraction directly

Here we create small machines that can be parts of many bigger machines.
Bigger machines can CREATE these smaller machines (Procedures as output / procedures that return procedures). 
Bigger machines can also USE these smaller machines as parts (Procedures as input / arguments). 

Procedures passed here express common programming patterns we find / discover and see they’re useful in many cases so we decide to abstract them and use them as parts.

## Summary of the core ideas this week

In week 1 we learned about recursion:\
Think writing programs that are function compositions in the form f(g(x)). 
Recursion is best suited for this because u can keep calling functions like: 
$f(f(f(f(x))))$ till u get to basecase. 

This week we learn about “Procedures as data” (or what I call everything is a machine) : 

To build composite programs using the methods above and allow them not just to use the same function $f$ over and over again but to express complex ideas, we can make functions that take other functions as input and returns functions as outputs (like a machine that gets inputted into another machine to add parts to it and make it more complex to produce a new machine part). 

The core example here is derivatives and integrals from calculus:
A derivative is a function that takes as input function f(x) and produces another function f’(x)

$$ D (g(x)) = \frac {(g(x + dx) - g(x))}  {dx} = g’(x) $$

```
g(x) -[D]-> g’(x)
```

## Procedures as arguments / inputs


```
#lang racket
(require berkeley)


(define (inc x) (+ x 1))
(define (identity x) x)
(define (pi-term x) (/ 1.0 (* x (+ x 2))))
(define (pi-next x) (+ x 4))
(define (cube x) (* x x x))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-ints a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (sum pi-term a pi-next b))

; we can rewrite pi-sum using lambda
(define (pi-sum2 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a (lambda (x) (+ x 4)) b))
```

## lambda, let syntactic sugar

suppose we have the following function 
$$f(x, y) = x(1 + xy)^2 + y(1-y) + (1+xy)(1-y)$$

in math we do something like
let


$$a = 1 + xy$$
$$b = 1 - y$$

and we can rewrite our function as:

$$f(x, y) = xa^2 + yb + ab$$

How can we write this function in LISP? 
$$f(x, y) = x(1 + xy)^2 + y(1-y) + (1+xy)(1-y)$$


3 ways:

1- 
```
define f(x,y):
	define f-helper(x,y):
		returns xa^2 + yb + ab
	return f-helper( (1+xy), (1 - y))
```

2- we can remove the local name using lambda 
```
define f(x, y):
	( lambda (a, b) (xa^2 + yb + ab) ) -> apply to ( 1 + xy, 1 - y) 
```
3- This costruct of calling lambda this way turns out to be so common such that there became some syntacitc sugar (special form) for it:

```
define f(x, y):
	let a -> (1 + xy)
	     b -> ( 1 - y)
	then do (body)
	(xa^2 + yb + ab)
```

What is the difference? 

```
( lambda <var> <body> <expression of var that body executes>) 
( let <var> <expression of var> <body call on expressed var>) 
```
let is more similar to:
```
define f(x,y):
	define a -> (1 + xy)
	define b -> (1 - y)
	body: (xa^2 + yb + ab)
```

lambda syntax: `( lambda ( < parameters> ) <body> ) `

let syntax:
```
(let ( 
	(<var1> <exp1>)
	(<var2> <exp2>)
	…..
	)
	<body>
) 
```

## Procedures as returned values: the example of average damping

How does this allow more expressive power? 

Consider the example of computing sqrt(x). What’s our algorithm? 

```
start with initial guess
if good-enough? return it
else try next-guess -> average (guess , x/guess) 
```

```
#lang racket
(require berkeley)

(define (average a b)
  (/ (+ a b) 2.0))

(define (good-enough? v1 v2)
	(< (abs (- (square v1) v2)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x) x)))


; what is the new guess? average (x + x/y) where x/y = y = f
; suppose y = sqrt(x) ; f(x) = sqrt(x); y = f(x) ; y^2 = x ; y.y = x ; y = x/y
; f(x) = x/y; f(guess) = x/guess
; so what improve is doing is averging x and some f(x) = x/y
; the heart of sqrt procedure is just repeatedly applying f until it satisfies some criterion

(define (close-enough? v1 v2)
  (< (abs (-  v1 v2)) 0.00001))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; we can thus redefine sqrt as a repeat application of the fixed point procedure
(define (sqrt2 y x)
  (fixed-point (lambda (y) (average y (/ x y))) x))
; in this version, fixed-point takes a function (given by lambda expression) as input

; this functionality of averaging some x and some f(x) can be a procedure called average damping
(define (average-damp f)
  (lambda (x) (average x (f x))))
; average-damp is a procedure that takes as input a function f and returns a procedure that
; when applied to a number x it returns an average of x & f(x)

; we can now get v3 of sqrt
(define (sqrt3 y x)
  (fixed-point (average-damp (lambda (y) (/ x y))) x))
```
The idea is now expressed differently (and arguably more clearly). We repeatedly improve our guesses (fixed-point) via average-damping (averaging guesses and f(guesses)). 

Why are we able to do this with functions?\ 
We can only do this because the language allows us to. The language allowed us to:
- Give names to procedures / functions
- Pass them as arguments to procedures
- Have them be returned as results of other procedures
- Later we’ll see they can be included in data structures

If some computational element of the language has these abilities, they’re said to have first-class status. 

The ability to use procedures as data lets us build any control mechanism we want


## Let and Lambda (example from CS61A notes)

We write a function that returns a sentence containing the two roots of the quadratic equation ax2 +bx+c = 0 
using the formula 
$$x = \frac{(−b ± \sqrt{b^2 − 4ac})} {2a}$$ `

Could be written as: 
```
(define (roots a b c) 
	(se (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) )) 
```

But we compute the sqrt twice. So we could make it to:
```
(define (roots a b c) 
	(define (roots1 d) 		(se (/ (+ (- b) d) (* 2 a)) 
			(/ (- (- b) d) (* 2 a)) 
	(roots1 (sqrt (- (* b b) (* 4 )) a c)))) ) 
```
roots -> f
roots1-> f1
sqrt -> f2

This structure we have is: 
```
f(a, b, c):
	f1(d)
   f1(d = f2(a, b, c))
```
where d is given by f2
ie:
```
f(a, b, c):
	f1( d )
   f1(f2)
```

But if we use a function once (like f1), no need to give it a name
```
f(a, b, b)
	(lambda (d) |-> (………)) (f2)
	‘’’call this function 	   ‘’’ on this argument
```
but this is difficult to read\
and the connection between “d” and the expression that computes its value “f2” isn’t immediately obvious\
and the order of computation isn’t the order of the expressions in the function

A fix for this is “let”. “let” calls lambda implicitly but provides the reader with a nicer expression to read
```
( let (value) (what computes the value)
	(body of function where computed value is used))

f(a, b, c):
	let (d) (what computes it) 
		(use it to do sth here)
```

remember that let does not provide any new capabilities; it’s merely an abbreviation for a lambda and an invocation of the unnamed function. 


The two versions: 
```
(define (roots a b c)
(se (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
    (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) ))

(define (roots2 a b c)
  (let ( (d (sqrt (- (* b b) (* 4 a c))))
         (-b (- b))
         (2a (* 2 a )))
    (se (/ (+ -b d) 2a)
        (/ (- -b d) 2a)
        )))
```

## A Note on lambda and math:

The expression $ax+b$ is not a function but a number. 

If u want to talk about the function $f(x) = ax + b$ real mathematicians would say

$$x \mapsto ax + b$$

Why express it this way? 

Suppopse we have the expression $xy+z$. How do we know if this is a function of x or y or z or y and z or .... 

Maybe it is 

$$y \mapsto xy + z$$

$$ {x, y, z} \mapsto xy + z$$

$$ {x, z} \mapsto xy + z$$

and so on

$ \mapsto $ is not in the keybopard so we used λ : lambda

λx means a function of x







