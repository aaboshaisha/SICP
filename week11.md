# W11 Streams

## An alternative approach to modeling state:

How to avoid “time” in modeling systems?

We shift our perspective.
How?

Before we modeled some aspect of a phenomenon that changes in time via some state variable x that could change value (object-oriented paradigm).

But, if we model that aspect of the phenomenon (the behavior of the quantity x) NOT as a state variable but as a mathematical function of time ue: x |-> x(t), the function itself does not change (timeless).

So, we shift our perspective from the variable to the entire history of values (a signal processing paradigm).

This is the Functional representation of time-varying information (versus OOP).

The entire history of values implies we have a “sequence”.

Previously we implemented sequences using lists.

Implementing the “entire history of values” (signal) with a list will be very inefficient. Why?
Recall previously when we implemented the signal-processing paradigm in Week 5 using `map`, `filter`, `accumulate`. Consider the following example (from the notes):


```scheme
(define (prime? n)
  (null? (filter (lambda (x) (= (remainder n x) 0)) (range 2 (- n 1)))))
```

But we don’t usually program it that way. Instead, we write a loop:

```scheme
(define (prime? n)
  (define (iter factor)
    (cond
      ((= factor n) true)
      ((= (remainder n factor) 0) false)  ; this allows us to stop early if we found a factor
      (else (iter (+ factor 1)))))
  (iter 2))
```

Why don’t we write it the way we expressed the problem in words? The problem is one of efficiency. Let’s say we want to know if 1000 is prime. We end up constructing a list of 998 numbers and testing all of them as possible factors of 1000, when testing the first possible factor would have given us a false result quickly.

The problem with the `iter` version is it’s hard to read and understand. If we named the procedure `unknown` instead of `prime?`, u’ll have a hard time figuring out what it does. Even keeping the name it’s still hard.

Can we get the best of both worlds? Can we have that nice signal-processing readable code and also make it fast enough like the `iter` version?

The answer is “Yes” vis `streams`.

The idea of streams is simple: don’t compute the whole list. Compute only as needed.

“Streams” (a kind of sequence that allows “delayed” evaluation) where the `car` of a sequence is the element itself while the `cdr` is a “promise” to compute the next element only if needed (recall Python generators).

From the notes: A list is implemented as a pair whose car is the first element and whose cdr is the rest of the elements. A stream is almost the same: It’s a pair whose car is the first element and whose cdr is a promise to compute the rest of the elements later.

Streams are delayed lists.

Summary: What we’ve accomplished is to decouple the form of a program—the order in which computations are presented—from the actual order of evaluation (the program as written makes it look like we have the whole sequence of numbers but the actual events that take place in the machine are computation as needed).

This is one more step on the long march that this whole course is about, i.e., letting us write programs in a language that reflects the problems we’re trying to solve instead of reflecting the way computers work.

This trick is achieved via making the computation that does this the “body” of a lambda procedure that could be called upon when needed.

Delayed evaluation is -> demand-driven programming.

From the notes:

```scheme
(cons-stream a b) ==> (cons a (delay b))
```

`delay` is itself a special form, the one that constructs a promise. We can represent a promise as a function. So the expression

```scheme
(delay b) ==> (lambda () b)
```

We use the promised expression as the body of a function with no arguments.

Once we have this mechanism, we can use ordinary functions to redeem our promises:

```scheme
(define (force promise) (promise))
```

`force` simply calls the procedure.

And now we can write the selectors for streams:

```scheme
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
```

Notice that forcing a promise doesn’t compute the entire rest of the job at once, necessarily. For example, if we take our range [2, 999] and ask for its tail, we don’t get a list of 997 values. All we get is a pair whose car is 3 and whose cdr is a new promise to compute [4, 999] later.

Since in many applications we may end up “forcing” the same delayed object many times, we make our programs more efficient by memoizing the computed values:

```scheme
; `memo-proc` takes a procedure and returns a memoized version of it

(define (memo-proc proc)
  (let ((already-run? false) ; define already-run? var (default is false)
        (result false)) ; define result var (default false)
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc)) ; compute the result by running proc and store it
            (set! already-run? true)
            result ; return result
            )
          result))))

So, to delay the evaluation of a proc, put its body as the body of a lambda exp
;;(define (delay exp)
  ;(memo-proc (lambda () (exp)))) ; returns a delayed object

; to force the evaluation of proc, just call it (it’s a lambda expression)
(define (force delayed-object)
  (delayed-object))
```

## Infinite sequences:

Because we compute values only as needed, we can use streams to represent sequences that are infinitely long.

; From the notes
Infinite streams. Think about the plain old list function

```scheme
(define (range from to)
  (if (> from to)
      '()
      (cons from (range (+ from 1) to))))
```

When we change this to a stream function, we change very little in the appearance of the program:

```scheme
(define (stream-range from to)
  (if (> from to)
      THE-EMPTY-STREAM
      (cons-STREAM from (stream-range (+ from 1) to))))
```

But this tiny above-the-line change makes an enormous difference in the actual behavior of the program.

Now let’s cross out the second argument and the end test:

```scheme
(define (stream-range from)
  (cons-stream from (stream-range (+ from 1))))
```

This is an enormous above-the-line change! We now have what looks like a recursive function with no base case—an infinite loop.
And yet there is hardly any difference at all in the actual behavior of the program.
The old version computed a range such as [2, 999] by constructing a single pair whose car is 2 and whose cdr is a promise to

 compute [3, 999] later.
The new version computes a range such as [2, ∞] by constructing a single pair whose car is 2 and whose cdr is a promise to compute [3, ∞] later!

This amazing idea lets us construct even some pretty complicated infinite sets, such as the set of all the prime numbers.

There are two styles for defining them:

```scheme
(define (integers-starting-from n) ; explicit definition
  (cons-stream
   n
   (integers-starting-from (+ n 1)))) ; the next computation explicitly defined

; to define an infinite stream of ones:

(define ones (cons-stream 1 ones)) ; notice the cdr is a promise to evaluate ones.

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))
```

Implicit: no explicit recursive call. Instead, there’s a call to `stream-map` (which is the thing that is recursive).

## Functional vs Object-oriented views of time:

The main problem we were trying to solve is how to model systems that change state.

In the OOP view, we model objects in the world by computational objects.
We model their state is modeled via “state” variables. We model the “change” in their state via assignment. Temporal behavior in the world is modeled by temporal behavior in the computer.
This view of the world is like the view from the perspective of the subject / user. We can only see locally so we have present, past and future.

The functional view of streams (signal processing) lets us consider the view from the perspective of a “God’s eye” like view that sees the “entire” history of state values as one stream of values. We therefore don’t see a state that changes but just one stream that is “stateless” and “timeless”. This can therefore be modeled in a functional style with a function whose input is fully determined by its output.

The book gives the example of the implementation of a “withdrawal processor” that monitors the balance in a bank account.

```scheme
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
```

“Calls to make-simplified-withdraw produce computational objects, each with a local state variable balance that is decremented by successive calls to the object. The object takes an amount as an argument and returns the new balance. We can imagine the user of a bank account typing a sequence of inputs to such an object and observing the sequence of returned values shown on a display screen.”

“Alternatively, we can model a withdrawal processor as a procedure that takes as input a balance and a stream of amounts to withdraw and produces the stream of successive balances in the account:

```scheme
(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw 
    (- balance (stream-car amount-stream))
    (stream-cdr amount-stream))))
```

`Stream-withdraw` implements a well-defined mathematical function whose output is fully determined by its input.

Suppose, however, that the input amount-stream is the stream of successive values typed by the user and that the resulting stream of balances is displayed. Then, from the perspective of the user who is typing values and watching results, the stream process has the same behavior as the object created by make-simplified-withdraw. However, with the stream version, there is no assignment, no local state variable. Yet the system has state!”

This functional style doesn’t always work, however. In fact, there are cases where it will run into the same problems of the object style it was trying to avoid. A key troublesome area arises when we wish to design interactive systems, especially ones that model interactions between independent entities.

This is illustrated by the example of trying to implement a banking system that allows “joint” accounts (A & B):

In the stream view, each account was a stream process that takes input a stream of transaction requests and produces a stream of responses.
To make a “joint” account, we could “merge” streams from A & B then feed the merged stream to a `bank-account` stream that produces the final result.
But, how do we “merge”? One might say we can simply “alternate” (e.g., A, B, A …etc). But suppose A uses the account rarely. Do we force B to “wait” and prevent it from issuing a transaction until A has made one? The two transaction streams must be “interleaved” in some way that is perceived by A and B to make sense / agree to what happens in “real time”.

The need to merge inputs from different agents reintroduces the same problems that the functional style was meant to eliminate.

