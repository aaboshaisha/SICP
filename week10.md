# W10 Concurrency 

The main problem we’re dealing with this week is: how can we deal with computations running in parallel? 

Before we intorduced assignment, our programs were timeless (any expression that has a value always has the same value).

Remeber one of the core goals of SICP is managing complexity. We need strategies to help us structure large systems so that they are modular ie that they divide “naturally” into parts that can be separately developed and maintained. 

One powerful design strategy, which is particularly appropriate to the construction of programs for modeling physical systems, is to: base the structure of our programs on the structure of the system being modeled:
- For each object in the system, we construct a corresponding computational object. 
- For each system action, we define a symbolic operation in our computational model. 

One such strategy is viewing a large system as a collection of distinct objects whose behaviors may change over time (OOP).
To make OOP possible, we needed to introduce “assignment” into our programs.
Assignment means values “change” and change means we now introduce “time” into our models. 
Objects in the real world do not change one at a time. We perceive them as acting concurrently “all at once”.
We want to be able to model these real world systems as objects that evolve “seprarately” and also “concurrently”.

Also, consider the following real world systems we might want to model:
- A banking system where people can have shared accounts and they want to be able to use them at the same time to deposit / withdraw and they end up with the right amounts of money in their wallets and the account.
- (Airline) reservation system where people can book seats on the same flight without conflicts (no double booking). This is also the case for any database (log / book keeping) system for keeping track of any sorts of transactions.

Here and in many other cases, parallelism is very important for speed and maybe even unavoidable. 
Multiple processors implementing some computation is the way our computers become faster. If it’s possible to decompose a problem into parts that can be computed independently and need only communicate rarely, we can take advantage of multiple cores of processing and gain tremendous speed.

However, Many things we take for granted in ordinary programming become problematic when there is any kind of parallelism involved.

The key problem we’ll deal with is “interleaving”
 "interleaving" refers to the concurrent execution or mixing of operations from different threads, processes, or components. 

For example, consider two threads / procedures executing operations A, B, and C:
* Thread 1: A1, B1, C1
* Thread 2: A2, B2, C2

The interleaved execution might look like: A1, A2, B1, C1, B2, C2. This may not give the desired result. 

Consider the following example (from the notes):

To see in simple terms what the problem is, think about the Scheme expression
```scheme
(set! x (+ x 1))
```

As you’ll learn in more detail in 61C, Scheme translates this into a sequence of instructions to your computer. The details depend on the particular computer model, but it’ll be something like this:

```assembly
lw $8, x
addi $8, 1
sw   $8, x
```

- Load a Word from memory location x into processor register number 8.
- Add the Immediate value 1 to the register.
- Store the Word from register 8 back into memory location x.

Ordinarily, we would expect this sequence of instructions to have the desired effect. If the value of x was 100 before these instructions, it should be 101 after them.

But imagine that this sequence of three instructions can be interrupted by other events that come in the middle. To be specific, let’s suppose that someone else is also trying to add 1 to x’s value. Now we might have this sequence:

**my process**
```assembly
lw $8, x
addi $8, 1
```

**other process**
```assembly
[value is 100] [value is 101]

lw   $9, x
addi $9, 1
sw   $9, x

- [value is 100] [value is 101] [stores 101]

sw $8, x
```

The ultimate value of x will be 101, instead of the correct 102.

The general idea we need to solve this problem is the critical section, which means a sequence of instructions that mustn’t be interrupted. The three instructions starting with the load and ending with the store are a critical section.

Actually, we don’t have to say that these instructions can’t be interrupted; the only condition we must enforce is that **they can’t be interrupted by another process that uses the variable x**. It’s okay if another process wants to add 1 to y meanwhile (eg: you want to be able to restrict deposit and withdrawal on account X - so they are ordered not concucrent - but you want account Y to be able to do whateve it wants at the same time). 

The general problem addressed here is: several processes share a common state variable and they try to manipulate iot at the same time. The problem is we may not be able to control the order of assignments made by the different processes. 

We want this control to make sure we get the “right” result. 
We have to define what “right” result means.
A right result will be: ensure the concurrent system prooduces consistent result as if the processes had been run sequentially in some order (even if this is multiple correct results as long as they are consistent).

eg 
```
(+ x 1)
(* x 10)
```
If initial x -> 10
- could give: 110 (10 + 1 = 11 x 10 = 110)
- or : 101 (10 x 10 = 100 + 1 = 101)

How do we decide the right level of granularity for imposing the control we want (the critical sections)?

First, let’s consider how to impose control in general? 

One general mechanism for costraining interleaving of concurrent processes is called `serializer`.

A `serializer` looks something like `(serializer <p1> <p2> <p3> …..)` (we pass it one procedure at a time but this is just for illustration). It works by passing a “flag” to any of the procedures p1..pn. Whoever gets the flag runs as a block and can’t have its inner steps interleaved by other processes of that serializer. Once <p1> is done, <p2> can take the flag and run and so on. Any <p> not within this serializer can run interleave disregarding the ps here.

The flag passing steps will be something like:

```
ACQUIRE the flag
RUN the procedure
RELEASE the flag
```

In Scheme, each procedure is passed separately to `(serialzier …)` and it return the “protected” procedure (the original procedure + restriction on interleaving as discussed). 

eg 

(define x-protector (make-serializer))
(define protected-increment-x (x-protector (lambda () (set! x (+ x 1)))))

> x
100

> (protected-increment-x)

> x
101


From the notes:

There can be many different serializers, all in operation at once, but each one can’t be doing two things at once. So if we say

```scheme
(define x-protector (make-serializer))
(define y-protector (make-serializer))

(parallel-execute (x-protector (lambda () (set! x (+ x 1))))
                  (y-protector (lambda () (set! y (+ y 1)))))
```

then both tasks can run at the same time; it doesn’t matter how their machine instructions are interleaved. But if we say

```scheme
(parallel-execute (x-protector (lambda () (set! x (+ x 1))))
                  (x-protector (lambda () (set! x (+ x 1)))))
```

then, since we’re using the same serializer in both tasks, the serializer will ensure that they don’t overlap in time.

`parallel-execute` takes any number of arguments, each of which is a procedure of no arguments, and invokes them, in parallel rather than in sequence. (This isn’t a standard part of Scheme, but an extension for this section of the textbook.)

A serializer isn’t a special form, it can’t take an expression as an argument. Instead, we must give it a procedure that it can invoke.


NB: The reason we need serialization in the first place is that if a process makes more than one reference to the same variable, we want to be sure its value is consistent throughout that process.

Put another way: The main reason we need serializers is if a process has many steps and those steps involve accessing the same varaiable more than once. This means it can be interrupted (interleaved) by another process which may change that variable and so its value won't be consistent across the multiple access steps. A serializer is needed to protect that and make the many steps of the process into "one block" that can't be interrupted.

**4 main problems can occur with concurrency:**

- **Incorrect results:** This is the problem that serializers are supposed to address. The worst problem is if the same seat is reserved for two different people. Just as in the case of adding 1 to x, the reservation system must first find a vacant seat, then mark that seat as occupied. That sequence of reading and then modifying the database must be protected.

But, whate is the right level of protection? Too much and we get —> 

- **Inefficiency:** One very simple way to ensure correct results is to use a single serializer to protect the entire reservation database, so that only one person could make a request at a time. But this is an unacceptable solution; thousands of people are waiting to reserve seats, mostly not for the same flight. A single serializer for the entire database means if one person is trying to book a flight, everyone else logging into that system at the same time won’t be able to use it an will have to wait. Meanwhile, many servers with many cores of processors for that system are sitting idle and not made use of.

- **Deadlock:** Suppose that someone wants to transfer money from account A to account B. We have the following code for the transfer procedure:

```
((from-account-serializer (to-account-serializer transfer-procedure) from to)
```
Suppose we have two processes A & B each where A is trying to execute transfer from you -> me while B is trying to execute transfer from me -> you

|   A      |    B     |
|----------|----------|
| ACQ you  | ACQ me   |
| RUN transfer | RUN transfer |
| REL you  | REL me   |


What 
```
((from-account-serializer (to-account-serializer transfer-procedure) me you)
``` 
will run is:

```
ACQ me
ACQ you —> this will fail since the other process is running the opposite (see below)
RUN
REL you
REL me
```

```
((from-account-serializer (to-account-serializer transfer-procedure) you me)
``` 
will run is:

```
ACQ you
ACQ me —> this will fail since the other process is running the opposite (see below)
RUN
REL me
REL you
```

Now we see the problem. Both halt after step-1 since the other has acquire what the other wanted at step-2

One solution to this is:
-> sometimes there is a natural ordering to the processes and so we can use it: eg in flight booking, always book flight 1 then flight 2
-> if there is no natural order we can impose one: eg do the ban account with smaller id first.

- **Unfairness:** This isn’t an issue in every situation, but sometimes you want to avoid a solution to the deadlock problem that always gives a certain process priority over some other one. If the high-priority process is greedy, the lower-priority process might never get its turn at the shared data. (Remeber, computers do things in loops millions of times. If you have 10 processes and the first always wins, the others won’t get a chance). 


## Implementation: How to implement a Serializer 

As we said, A Serializer passes a flag to each procedure and whoever has the flag can run and others wait.

```scheme
(define (make-serializer)
  (let ((in-use? flase)) ;this is the flag
    (lambda (proc)
      (define (protected-proc . args)
        (if in-use?
            (begin
              (wait-a-while) ; Never mind how to do that.
              (apply protected-proc args)) ; Try again
            (begin
              (set! in-use? true) ; Don’t let anyone else in.
              (apply proc args) ; Call the original procedure.
              (set! in-use? false)))) ; Finished, let others in again.
      protected-proc)))
```

In this implementation, each serializer is an object with a state variable (the flag : `in-use?`). If the flag is set to false, we can’t use the serializer and we need to try again later (not given how above). Otherwise, we can apply the procedure and after finishing, set its flag to false so other serialized-procedures can be run.

From the notes:

This is a little complicated, so concentrate on the important parts. In particular, never mind about the scheduling aspect of parallelism—how we can ask this process to wait a while before trying again if the serializer is already in use. And never mind the stuff about apply, which is needed only so that we can serialize procedures with any number of arguments.

The part to focus on is this:

```scheme
(if in-use?
    ... ; wait and try again
    (begin
      (set! in-use #t)
      (apply proc args)
      (set! in-use #f)))
```

The intent of this code is that it first checks to see if the serializer is already in use. If not, we claim the serializer by setting in-use true, do our job, and then release the serializer.

The problem is that this sequence of events is subject to the same parallelism problems as the procedure we’re trying to protect! 

Amr —> another process can sneak in between the reading and writing of the `in-use?`

What if we check the value of in-use, discover that it’s false, and right at that moment another process sneaks in and grabs the serializer? In order to make this work we’d have to have another serializer protecting this one, and a third serializer protecting the second one, and so on.

There is no easy way to avoid this problem by clever programming tricks within the competing processes. We need help at the level of the underlying machinery that provides the parallelism: the hardware and/or the operating system. That underlying level must provide a guaranteed atomic operation with which we can test the old value of in-use and change it to a new value with no possibility of another process intervening. (It turns out that there is a very tricky software algorithm to generate guaranteed atomic test-and-set, but in practice, there is almost always hardware support for parallelism.
 
The textbook assumes the existence of a procedure called test-and-set! with this guarantee of atomicity. Although there is a pseudo-implementation on page 312, that procedure won’t really work, for the same reason that my pseudo-implementation of make-serializer won’t work. What you have to imagine is that test-and-set! is a single instruction in the computer’s hardware.



**The Mutex**

The book uses an intermediate level of abstraction between the serializer and the atomic hardware capability, called a mutex. 
What’s the difference between a mutex and a serializer? 
- The serializer provides, as an abstraction, a protected operation, without requiring the programmer to think about the mechanism by which it’s protected. 
- The mutex exposes the sequence of events. Just as my incorrect implementation said

```scheme
(set! in-use #t)
(apply proc args)
(set! in-use #f)
```

the correct version uses a similar sequence

```scheme
(mutex ’acquire)
(apply proc args)
(mutex ’release)
```

Amr: So, the `serializer` passes a flag that is the `mutex`. The `mutex` uses the `test-and-set!` procedure (for illustrative pruposes in the book but it’s usually a hardware / operating system taking care of it). 


By the way, all of the versions in these notes have another bug; I’ve simplified the discussion by ignoring the problem of return values. We want the value returned by `protected-proc` to be the same as the value returned by the original `proc`, even though the call to `proc` isn’t the last step. Therefore, the correct implementation is

```scheme
(mutex ’acquire)
(let ((result (apply proc args)))
  (mutex ’release)
  result)
```

as in the book’s implementation on page 311.






