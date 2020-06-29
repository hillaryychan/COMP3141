# Effects and State

## Effects

**Effects** are observable phenomena from the execution of a program.  
Examples:

* memory effects

    ``` c
    int *p = ...
    ... // read and write
    *p = *p + 1
    ```

* IO

    ``` c
    // console IO
    c = getchar();
    printf("%d", 32);
    ```

* non-termination

    ``` c
    // infinite loop
    while (1) {};
    ```

* control flow

    ``` c
    // exception effect
    throw new Exception();
    ```

An **external effect** is an effect that is ***observable*** outside the function.  
Examples: console, file and network IO; termination and non-termination; non-local control flow etc.
**Internal effects** are not observable from outside.

Memory effects are internal or external depending on the scope of memory being accessed. Global memory accesses are *external*

### Purity

A function with no external effects is called a **pure function**.

> A **pure function** is the mathematical notion of a function. That is, a function of type `a -> b` is *fully* specified from all elements of the domain type `a` to the codomain type `b`

Consequences:

* two invocations with the same argument result in the same value
* no observable trace is left beyond the result of the function
* no implicit notion of time or order of execution

Haskell functions are technically ***not pure***.  
They can loop infinitely, throw exceptions (partial functions), force evaluation of unevaluated expressions

Purity only applies to a particular level of abstraction. Even ignoring the above, assembly instructions produced by GHC aren't really pure.

Despite the impurity of Haskell functions, we can often reason as though they are pure. Hence we call Haskell a *purely funcitonal* language.

### The Danger of Implicit Side Effects

* They introduce (often subtle) requirements on the evaluation order.
* They are not visible from the type signature of the function
* They introduce **non-local** dependencies which is bad for software design, increasing *coupling*
* They interfere badly with strong typing, for example mutable arrays in Java, or reference types in ML

We can't, in general, *reason equationally* about effectful programs

### Programming Pure Functions

It is possible to program pure functions in Haskell.

Typically, a computation involving some state of type `s` and returning a result of type `a` can be expressed as a function: `s -> (s, a)`  
Rather than *change* the state, we return a **new copy** of the state.

All that copying might seem expensive, but by using tree data structures, we can usually reduce the cost to an O(log n) overhead.

## State

Example: labelling notes in a tree in ascending number in infix order

``` hs
data Tree a = Branch a (Tree a) (Tree a) | Leaf
label :: Tree () -> Tree Int
```

![tree labelling](../imgs/05-8_tree-labelling.png)

We will use a data type to simplify this:  
`newtype State s a =` is a **procedure** that, manipulating some state of type `s` returns `a`

State operations:

``` hs
get :: State s s
put :: s -> State s ()
pure :: a -> State s a
evalState :: State s a -> s -> a
modify :: (s -> s) -> State s ()
```

Sequential composition:

``` hs
-- Do one state action after another with `do` blocks:
do put 42
   pure True
-- desugars
put 42 >> put True
(>>) :: State s a -> State s b -> State s b
```

Blind:

``` hs
-- The second step can depend on the first step with bind
do x <- get
   pure (x+1)
-- desugars
get >>= \x -> pure (x + 1)
(>>=) :: State s a -> (a => State s b) -> State s b
```

### State Implementation

The `State` type is essentially implemented as the same state-passing we did before.

``` hs
newtype State s a = State (s -> (s,a))
```

TODO - implement state operation for newtype

In the Haskell standard library `mtl`, the `State` type is actually implemented slightly differently, but the implementation essentially works the same way.
