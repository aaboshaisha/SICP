Certainly! Here's the provided text formatted in Markdown:

```markdown
# W5- Picture Language: Abstraction and Stratified Designs

## Core Idea: Abstraction

Procedures are machines that do things. Two forms of abstraction give us tremendous abilities when building programs:

1. **Procedure Abstraction:** Detect common patterns in programs and implement them using higher-order procedures. For example, the "accumulation" pattern:

```scheme
(define (accumulate combiner initial items)
  (if (null? items)
      initial
      (combiner (car items) (accumulate combiner initial (cdr items)))))
      
(accumulate + 0 (list 1 2 3 4))
```

2. **Data Abstraction:** A data type is defined by the set of interface operations, and we don't care about the implementation. Change in implementation (representation) is possible while maintaining the same interface.

    Example: A number is something we can + - * /, and we don't care about its internal representation. It could be 'I, II, IV' or '1, 2, 4'.

## Stratified Design

**Stratified Design:**
- Complex systems are built as a sequence of levels using a sequence of languages.
- Each level combines parts that become the primitives for the next level.
- The language at each level has primitives, means of combination, and means of abstraction.

**Example: Computer Engineering**
Resistors and transistors -> Logic gates (AND, OR, NAND) -> Processors, bus structures, memory systems -> Computers using computer architecture language -> Distributed systems using network language.

### How it Manifests in the Picture Language

We have a language for lines and points, defining vectors and operations on them to make line segments:

```scheme
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (make-segment start-point end-point)
  (cons (make-vect 0 start-point) (make-vect 0 end-point)))

(define (start-segment segment) (cdr (car segment)))
(define (end-segment segment) (cdr (cdr segment)))
```

These points and line segments are used to build painters using `(segments->painter segment-list)`. Painters have basic means of combination (`beside` and `below`).

```scheme
(define (split f1 f2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f1 f2) painter (- n 1))))
          (f1 painter (f2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
```

With data abstraction, the implementation details at each level don't matter. For example, making a frame using vectors:

```scheme
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
```

or

```scheme
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
```

The next painter using the frame as an argument doesn't care about the implementation details, as long as the selectors `(origin-frame frame)`, `(edge1-frame frame)`, and `(edge2-frame frame)` give the right results.

Stratified design makes programs robust: small changes in specifications require correspondingly small changes in the program. Changes in the final output can be made at different levels (low-level painter, mid-level splitting procedures, or high-level ones such as `square-of-four`).

