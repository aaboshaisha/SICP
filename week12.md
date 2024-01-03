# W12 part-1: Interpretation (part 2 of the book)

We'll start the study of interpretation by building a simple example of a calculator program.

## The calculator program:

### Example of its use:

```
calc: (+ 2 3)
5
```

The entire program consists of three procedures in 30 lines of code. You should find it easy to understand. And yet, these three procedures exactly parallel the core procedures in a real Scheme interpreter:

1. **The read-eval-print loop:** Interact with the user.
2. **Eval:** `(eval expression)` returns the value of the expression. (exp -> value)
3. **Apply:** `(apply function argument-values)` calls the function and returns the value it returns.

### Helper procedure to be used later:

```scheme
(define (accumulate proc initial items)
  (if (null? items)
      initial
      (proc (car items) (accumulate proc initial (cdr items)))))
```

## 1- Applying procedure to args

```scheme
(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
        ((eq? fn '*) (accumulate * 1 args))
        ((eq? fn '-)
         (cond ((null? args) (error "Calc: no args to - "))
               ((= 1 (length args)) (- (car args))) ; additive inverse
               (else (- (car args) (accumulate 0 + (cdr args))))))
        ((eq? fn '/)
         (cond ((null? args) (error "Calc: no args to / "))
               ((= 1 (length args)) (/ (car args))) ; multiplicative inverse
               (else (/ (car args) (accumulate * 1 (cdr args))))))
        (else (error "Calc: Bad operator:" fn))))
```

**APPLY** knows nothing about syntax. It's entirely in the world of values. Its arguments are procedure and argument values. Its work is all about semantics.

## 2- Evaluate expressions:

Expression typed by the user is represented as a list by Scheme's "read," which reads keyboard input from the user.

The expression in the calculator program / language could be:
1. A number
2. A function (+ - / *) (procedure call represented by a list)

Scheme (language - one that speaks to an interpreter) has 4 types of expressions:
1. Self-evaluating: numbers and booleans
2. Variables
3. Function calls
4. Special forms

In our calculator language, a user can type some expressions, and we're writing an interpreter for them (calc.scm). We have only 2 out of the 4 types above.

Notice, the `calc-apply` procedure is different from a real Scheme interpreter. In Scheme, since functions are first-class citizens, what is passed to apply is the <procedure> itself, not a “symbol” as in calc-apply.

```scheme
(define (calc-eval exp) ; translate expression in our "language" to computable values
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp)))) ; procedure call - each list is a procedure call since there are no special forms
        (else (error "Calc: Bad expression:" exp))))
```

**What EVAL does:** figures out what the notation means. EVAL knows syntax. It converts syntax to semantics (turns the form of your program into something meaningful - that’s a request to compute some value).

Also, notice the recursive call in `(map calc-eval (cdr exp)`. Consider what happens when calc-eval is called in `(calc-eval '(+ (+ 3 4) (* 5 (+ 6 7))))`.

Notice how calc-eval is called recursively for each subexpression:

```scheme
> (calc-eval '(+ (+ 3 4) (* 5 (+ 6 7))))
> (calc-eval '(+ 3 4))
> > (calc-eval 3)
< < 3
> > (calc-eval 4)
< < 4
< 7
> (calc-eval '(* 5 (+ 6 7)))
> > (calc-eval 5)
< < 5
> > (calc-eval '(+ 6 7))
> > > (calc-eval 6)
< < < 6
> > > (calc-eval 7)
< < < 7
< < 13
< 65
< 72
```

`(map calc-eval (cdr exp)` is the key to dealing with deep lists. Notice here the exploration is vertical and horizontal.

**Horizontal:** this is when map looks at each of its arguments till it gets to its base case which is ‘() empty list.

**Vertical:** in the code:

```scheme
(define (calc-eval exp) ; translate expression in our "language" to computable values
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp)))) 
        (else (error "Calc: Bad expression:" exp))))
```

when exploring each list (argument) in itself.

**Normal vs. Applicative order reflected in the interpreter:**
Why do we do:
`(calc-apply (car exp) (map calc-eval (cdr exp)))`
and not
`(calc-apply (car exp) (cdr exp))`
??

Remember:
- Applicative order: first get the values of argument expressions then apply the function (recursively evaluate the arguments before we call apply) as in `(calc-apply (car exp) (map calc-eval (cdr exp)))`
- Normal order: get the values only when needed (we give the actual argument “expressions” to the function as in `(calc-apply (car exp) (cdr exp))`

Scheme uses applicative order; therefore, we do the first.

## 3- The read-eval-print loop:

```scheme
(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read))) ; read the exp user typed -> evaluate its value -> print it
  (newline)
  (calc))
```

In LISP, the form of the “expression” matches the form of the “data” the language knows how to deal with. One complete expression is one list. It was designed to be able to evaluate its own programs very straightforwardly.

**Why is the calculator program a language? What is a language?**
The syntax and semantics that allow us to express ideas. There are ideas we can express in one language that we can’t express in another. For example, the calculator above is a limited language in which we can express ideas of addition, subtraction, multiplication, and division. We can’t express conditionals or variables (while in Scheme we can).

## Interpreters and programming

**Q1- Is what we’re doing when writing programs is inventing languages and writing interpreters for them?**
When we write a program, we want to express some ideas in a way in which a machine can understand and execute them. The same thing is the programming language itself, which expresses some ideas (loops, conditionals, etc.) for a machine to execute. In this case, the translation is carried out by an interpreter. In the first case, it’s the programming language that is the intermediary between us and the machine.

**Q2- Does that change anything about the way we write code?**

(Universality of programmable computers)
Universality: we can write ONE computer program (the interpreter) which can do ANY computation by taking as data the formal description of the algorithm you want to use (a description of the machine that does the computation - the action you want to do is presented to the universal machine as data). A Scheme interpreter takes as input a Scheme program and turns it into whatever result you want.

# W12 part-2: Interpretation

We started this book by looking at programming and writing programs as a way of expressing our ideas about the "how-to" do some computations using a language the machine can understand (our programming language of choice was Scheme).

In expressing those ideas of the how-to, we were building small machines (procedures) and as we put them together to achieve our goals, we were building complex (computational) systems. To control the complexity of our designs, we had to use the same general techniques used by all designers of complex systems to tame complexity:

1. Combine primitive elements to form compound objects (small machines to make large machines).
2. Abstract compound objects to form higher level building blocks.
3. Preserve modularity by adopting the appropriate large-scale view of our system's structure (e.g., seeing it as objects with state interacting in time or stateless signal-processing system).

Recall our previous discussion of languages and the systems they describe.
- Complex systems are built as a sequence of levels using a sequence of languages.
- Each level is constructed by combining parts that become the primitives for the next level.
- The language used at each level will have: 1. primitives 2. means of combination 3. means of abstraction.

**Example: Computer Engineering**
Resistors and transistors described using a language of analogue circuits
-> combined to produce parts such as logic gates (AND, OR, NAND) that form the primitives of a language for digital circuit design
-> combined to build processors, bus structures, and memory systems
-> combined to form computers using the language of computer architecture
-> computers can be combined to form distributed systems using network language and so on.

We find we must constantly invent new languages to express our ideas. When going from one level to another, our language changes, we do the combination and abstraction above, and thus we're capable of expressing different ideas and for different purposes (the network language to describe neurons connected is different from the circuit language to describe the same neurons).

**How do we move between levels?**
-> When we express ideas to computers in one language, eventually what we want is for the machine to execute them. So, there must be a way / a machine / a program that takes our descriptions and allows the machine to perform the actions required. This is called the "interpreter".

Think about this: we move from transistors -> logic gates -> CPU and memory chips and instead of writing in Binary, we endow them with Machine Language but we don't want to write in it, so we write in another language (Scheme or Python) and have another program (the interpreter) to translate and carry out the actions using Machine Language.

**The evaluator that determines the "meaning" of expressions in a programming language is just another program.**

We can view all programming this way. When we write a spreadsheet program or a photoshop program or animation program or biological simulation program, we're essentially designing a new language (the spreadsheet or animation language) and writing an evaluator (our program) that translates it to the programming language which in turn uses another evaluator to translate it to the machine and so on.

**"Almost ANY program is the evaluator of some language."**

**"Computer science itself becomes the discipline of constructing appropriate descriptive languages."**

**Programs as descriptions of machines - programs as data:**

We said the evaluator determines the "meaning" of expressions of a programming language.
What does "meaning" here mean?
-> An operational view of the "meaning" of a computer program is that a program is a description of an abstract machine.

**From the book: consider the factorial program:**

```scheme
(define (factorial n) 
  (if (= n 1) 1 (* (factorial (- n 1)) n)))
```

We may regard this program as the description of a machine containing parts that decrement, multiply, and test for equality, together with a two-position switch and another factorial machine. (The factorial machine is infinite because it contains another factorial machine within it.)

![Image-1](w12-img1)

Similarly, the evaluator becomes a very special machine that takes as input a description of a machine. Given this input, the evaluator configures itself to emulate the machine described. For example, if we feed our evaluator the definition of factorial, the evaluator will be able to compute factorials.

![Image-2](w12-img2)

This is the idea of universality: we can write one program (make one machine) that is equivalent to / could emulate all other machines. This single machine requires we provide it as input the formal description of other machines (accepts other machines / programs as data).

The example we'll study here is an evaluator for Scheme built in Scheme.

**The Scheme evaluator:**

-> Our evaluator reduces expressions ultimately to the application of primitive procedures.

Here is the half-page version that captures its essence:

```scheme
(define (scheme)
  (display "> ")
  (print (eval (read) the-global-environment))
  (scheme))

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((symbol? exp) (lookup-in-env exp env))
    ((special-form? exp) (do-special-form exp env))
    (else
      (apply (eval (car exp) env)
             (map (lambda (e) (eval e env)) (cdr exp))))))

(define (apply proc args)
  (if (primitive? proc)
      (do-magic proc args)
      (eval (body proc)
            (extend-environment (formals proc) args (proc-env proc)))))
```

The essential structure includes mutual recursion between:
->  `eval` (to evaluate an expression relative to an environment) and 
-> `apply` (to apply a function to arguments).

Evaluating a compound expression involves recursively evaluating its subexpressions and then applying the `car` (a function) to the `cdr` (the arguments).

Applying a function to arguments means evaluating the body of the function in a new environment.

EVAL knows syntax. It converts syntax to semantics (turns the form of your program into something meaningful - that's a request to compute some value).

APPLY knows nothing about syntax. It's entirely in the world of values. Its arguments are procedure and argument values. Its work is all about semantics.

Eval classifies expressions / does case analysis of the syntactic type of the expression to be evaluated.

The syntax of the language being evaluated, therefore, will be determined by the procedures that classify and extract pieces of expressions.

Apply classifies procedures into two types:
-> primitive
-> compound (user-defined using lambda)

- Primitives are called via some means to apply machine language on args.
- Compounds are handled via eval until they become primitives.
