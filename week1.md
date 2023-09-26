## Week1 Summary and Reflections: 
---------------------------------

This week we start our discussion of "computational processes" and of the programming language LISP (a certain dialect of it Scheme). 

Math vs Programming: Math (as a language) is concerned mostly with describing "what is x" aka declarative knowledge. Programming languages are concerned with describing "how to do x" aka imperative knowledge. An illustrative example is the mathematical function $\sqrt{x} = y$ such that $y >= 0$ and $y^2 = x$.

This function tells us “What” a square root is. We can use it to decide whether some number we’re given is a square root of another. We can use it to derive other facts about square roots in general. 

BUT: It tells us NOTHING about how-to actually compute the square root of a given number. 

Contrast this to the following programming "procedure":

```
1. Initialize a guess for the square root, let's call it 'guess'.
2. Set a tolerance level 'tolerance' for the accuracy of the result.
3. Repeat the following steps until the absolute difference between 'guess^2' and 'x' is less than 'tolerance':
   a. Calculate the next guess using the formula:
      guess = 0.5 * (guess + x / guess)
4. Return 'guess' as the approximate square root of 'x'.
```

This tells us "how-to" compute the square root of x. This is called a "procedure" which are descriptions of the "computational process" of finding sqrt.

A programming language gives us the means / framework to organizaing our ideas about computational processes. To be able to do so, any programming language must have:

1. primitive expressions: represents the simplest entites the language deals with (eg operations such as +, - or numbers 1, 2, 3 ... ie primitive data and primitive procedures) 
2. means to combine these primitive expressions into more complex ones. 
3. means of abstraction: now that we have combined some primitives into more complex entities, we want to give names to these new entities and use them as units of their own. 

Think of this as having some mechanical building blocks and you put them together to create little machines. Now, you can use the little machines to build even bigger machines. We can give the little machines names, and if they are well designed, you can use them to make bigger machines without having to know how they work inside (much like how you can use your toaster to make toast without needing to know how it's put together). This (putting things together, naming and using) is called abstrction (the CS definiton not the everyday use in natural languae).

----
LISP
----

We then get introduced to our programming language of choice "LISP". In it, we can see:

primitive expression (data) such as numbers 
> `3`
and primitive expression (procedure) such as 
> `+`
and we can combine expressions representing numbers with expressions representing primitive procedure (such as + or *) to form a compound expression 
> `(+ 3 4)`
in which `( )` say apply `+` to `3` and `4`

And we can give names to things (using `define`) such as giving names to variables:
> `(define pi 3.14)`

`define` is the language's simplest form of abstraction 

and we can combine operations to make a procedure and give it a name:
> `(define (square x) (* x x))`

To give names to things, there has to be some notion of memory such that these can be remembered. This gives us the notion of the programming "environment" which keeps this memory and gives meaning to names used in our program (there is no way to know what some `x` means without reference to the environment in which `x` is defined.

------------
Interpreters
------------
Interpreters take what we write in LISP and convert it to something a computer can use. How will interpreters evaluate our expressions? 

Two possible ways:
1. Go over each expression. Evaluate all operators and operands until we get an expression with only primitives. Now compute. This is called "Normal order" evaluation.
2. Go over the expression. Evaluate the operator and operands then apply as you go. 

In 2, we don't wait for all operators and operands in the whole expression before we can apply some. Whatever gets evaluated gets applied as we go.

LISP used "2".

NB: There are what are called "special forms" which get evaluated differently.

NB: Scheme doesn't evaluate expressions L -> R or R -> L but inside out.


-----------------------
Functions vs Procedures
-----------------------
In programming, we have to distinguish function and procedures:
- Functions: give same output for same input (arguments)
- Procedure: can give different output for same input depending on the "state" of variables in the current environment. 

It’s not a function unless you always get the same answer for the same arguments.

Why does it matter?

This makes a huge difference in the way we program.\
Computers have to do things in parallel in order to make it more efficient. If a part of your program depends on the "state" of some other part of the program, this can cause problems if we're not careful because of the parallelism. 

A "Functional" paradigm however avoids these issues since if each computation is independent of the past history of the overall computation, we can reorder the little computations and do parallelism without issue. 

To combine functions in this paradigm to make bigger ones we rely on composition of functions like $f(g(x))$ (instead of having a sequence of events). 

How to repeat things? 
> Recursion: a function or process calls itself in order to solve a problem by breaking it down into smaller, similar subproblems. Recursion is when something is defined in terms of itself.

Example: 

```
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```


See notes on exercise 1-6 for an example where normal vs applicative order matters for whether you're using a function or a procedure. 
