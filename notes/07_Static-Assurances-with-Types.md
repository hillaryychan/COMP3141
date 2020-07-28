# Static Assurances with Types

Methods of assurance:

![methods of assurance](../imgs/08-2_methods-of-assurance.png)

Static means of assurance analyses a program ***without running it***

## Static vs. Dynamic

Static checks can be **exhaustive**

> An exhaustive check is a check that is able to analyse all possible executions of a program.

However, some properties cannot be checked statically in general (halting problem), or are intractable to feasibly check statically (state space explosion).

Dynamic checks cannot be exhaustive, but can be used to check some properties where static methods are unsuitable.

## Types

Most static and all dynamic methods of assurance are **not** integrated into the compilation process.

* You can compile and run your program even if it fails tests
* You can change your program to diverge from your model checker model
* Your proofs can diverge from your implementation

Because type are integrated into the compiler, they cannot diverge from the source code. This means that type signatures are a kind of *machine-checked documentation* for your code.

Types are the most widely used kind of formal verification in programming today.

* They are checked automatically by the compiler
* They can be extended to encompass properties and proof systems with very high expressivity (see [here](08_Theory-of-Types.md))
* They are an **exhaustive** analysis

We will be looking a techniques to encode various correctness conditions inside Haskell's type system

## Phantom Types

> A type parameter is **phantom** if it does not appear in the right hand side of the type definition  
> `newtype Size x = S Int`

Let's examine each one of the following use cases:

* We can use this parameter to track what **data invariants** have been established about a value
* We can use this parameter to track information about the representation (e.g. units of measure
* We can use this parameter to enforce an **ordering** of operations performed on these values (**type state**)

### Validation

``` hs
data UG -- empty type
data PG
data StudentID x = SID Int
```

A **smart constructor** is a function that checks a condition in order to establish an invariant for a particular type. Since it is meant to replace a regular data constructor, we expect it to be *total*.

We can define a **smart constructor** that specialises the type parameter:

``` hs
sid :: Int -> Either (StudentID UG) (StudentID PG)
-- recall
data Either a b = Left a | Right b
```

and define functions:

``` hs
enrollInCOMP3141 :: StudentID UG -> IO ()
lookupTranscript :: StudentID x -> IO String
```

### Units of Measure

In 1999, software confusing units of measure (pounds and newtons) caused a Mars orbiter to burn up in atmospheric entry

``` hs
data Kilometers
data Miles
data Value x = U Int
sydneyToMelbourne = (U 877 :: Value Kilometers) -- tagged with type level distinction
losAngelesToSanFran = (U 383 :: Value Miles)    -- ^
```

In addition to tagging values, we can also enforce constraints on unit:

``` hs
data Square a
area :: Value m -> Value m -> Value (Square m)
area (U x) (U y) = U (x * y)
```

Note the arguments to `area` must have the same units

### Type State

Examples: A `Socket` can either be read to receive data, or busy. If the socket is busy, the user must first use the `wait` operation, which blocks until the socket is ready. If the socket is ready, the user can use the send operation to send string data, which will make the socket busy again.

``` hs
data Busy
data Ready
newtype Socket s = Socket ... -- the implementation is not important

wait :: Socket Busy -> IO (Socket Ready)
send :: Socket Ready -> String -> IO (Socket Busy)
```

The previous code assumes that we don't re-use old `Socket`s

``` hs
-- expected linear usage
send2 :: Socket Ready -> String -> String -> IO (Socket Busy)
send2 s x y = do s' <- send s x
                 s'' <- wait s'
                 s''' <- send s'' y
                 pure s'''
```

But, we can just re-use old values to send without waiting:

``` hs
send2' s x y = do _ <- send s x
                  s' <- send s x
                  pure s'
```

**Linear type** systems can solve this, but not in Haskell (yet)

### Datatype Promotion

Defining empty data types for our tags is **untyped**. We can have `StudentID UG`, but also `StudentID String`.

Recall that Haskell types themselves have types, called kinds. We can make our tag types more precise than `*` using the `DataKings` language extension, which lets us use data types as kinds:

``` hs
{-# LANGUAGE DataKinds, KindSignatures #-}
data Stream = UG | PG
data StudentID (x :: Stream) = SID Int
-- rest as before
```

## GADTS

### Motivation

For the given types:

``` hs
data Expr = BConst Bool
          | IConst Int
          | Times Expr Expr
          | Less Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
data Value = BVal Bool | IVal Int
```

If we were to define an expression calculator:

``` hs
data Expr t = BConst Bool
            | IConst Int
            | Times (Expr Int) (Expr Int)
            | Less (Expr Int) (Expr Int)
            | And (Expr Bool) (Expr Bool)
            | If (Expr Bool) (Expr t) (Expr t)
            deriving (Show, Eq)
data Value = BVal Bool | IVal Int
             deriving (Show, Eq)

eval :: Expr -> Value
eval (BConst b) = BVal b
eval (IConst i) = IVal i
eval (Times e1 e2) = case (eval e1, eval e2) of
                       (IVal i1, IVal i2) -> IVal (i1 * i2)
eval (Less e1 e2)  = case (eval e1, eval e2) of
                       (IVal i1, IVal i2) -> BVal (i1 < i2)
eval (And e1 e2)  = case (eval e1, eval e2) of
                       (BVal b1, BVal b2) -> BVal (b1 && b2)
eval (If ec et ee) =
  case eval ec of
    BVal True  -> eval et
    BVal False -> eval ee
```

The `eval` function is ***partial*** and undefined for input expressions that are not well-typed, like:

``` hs
And (IConst 3) (BConst True)
```

We can try adding a phantom parameter to `Expr`, and defining typed constructors with precise types:

``` hs
data Expr t = -- same as before

bConst :: Bool -> Expr Bool
bConst = BConst
iConst :: Int -> Expr Int
iConst = IConst
times :: Expr Int -> Expr Int -> Expr Int
times = Times
less :: Expr Int -> Expr Int -> Expr Bool
less = Less
and :: Expr Bool -> Expr Bool -> Expr Bool
and = And
if' :: Expr Bool -> Expr a -> Expr a -> Expr a
if' = If
```

This makes invalid expressions into type errors:

``` hs
-- couldn't match Int and Bool
and (iConst 3) (bConst True)
```

However inside `eval :: Expr t -> t`, Haskell type checker cannot be sure that we used our typed constructors so, in e.g. `IConst` case:

``` hs
eval :: Expr t -> t
eval (IConst i) = i -- type error
```

We are unable to tell that type `t` is definitely `Int`

### Generalised Algebraic Datatypes

**Generalised Algebraic Datatypes (GADTs)** is an extension to Haskell that, among other things, allows data types to be specified by writing the types of their constructors:

``` hs
{-# LANGUAGE GADTs, KindSignatures #-}
-- Unary natural numbers, e.g. 3 is S (S (S Z))
data Nat = Z | S Nat
-- is the same as
data Nat :: * where
  Z :: Nat        -- Zero
  S :: Nat -> Nat -- Successor (+1)
```

When combined with the type *indexing* trick of phantom types, it becomes very powerful

#### Expressions as a GADT

``` hs
data Expr :: * -> * where
  BConst :: Bool -> Expr Bool
  IConst :: Int -> Expr Int
  Times :: Expr Int -> Expr Int -> Expr Int
  Less :: Expr Int -> Expr Int -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
```

Now there is only **one** set of **precisely-typed** constructors

Inside `eval` now, the Haskell type checker accepts our previously problematic case:

``` hs
eval :: Expr t -> t
eval (IConst i) = i -- OK now
```

GHC now knows that if we have `IConst`, the type `t` must be `Int`

#### Lists as a GADT

We could define our own list type using GADT syntax as follows:

``` hs
data List (a :: *) :: * where
  Nil :: List a
  Cons :: a -> List a -> List a
```

But, if we define head (`hd`) and tail (`tl`) functions, they are partial:

``` hs
hd (Cons x xs) = x
tl (Cons x xs) = xs
```

We will constrain the domain of these functions by tracking the ***length*** of the list on the ***type level*** by creating Vectors (not the usual vectors)

As before define a natural number kind to use on the type level:

``` hs
data Nat = Z | S Nat
```

Now our length-indexed list can be defined, called a `Vec`:

``` hs
data Vec (a :: *) :: Nat -> * where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)
```

Now `hd` and `tl` can be total:

``` hs
hd :: Vec a (S n) -> a
hd (Cons x xs) = x
tl :: Vec a (S n) -> a n
tl (Cons x xs) = xs
```

Our map for vectors is as follows:

``` hs
mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec f Nil = Nil
mapVec f (Cons x xs) = Cons (f x) (mapVec f xs)
```

Using this type, it's impossible to write a `mapVec` function that changes the length of the vector.  
**Properties are verified by the compiler**!

### Trade-offs

The benefits of this extra static checking are obvious, however:

* It can be difficult to convince the Haskell type checker that your code is correct, even when it is
* Type-level encodings can make types more verbose and programs harder to understand
* Sometimes excessively detailed types can make type-checking very slow, hindering productivity

> **Pragmatism**
> We should use type-based encodings only when the ***assurance advantages outweigh the clarity disadvantges***  
> The typical use case for these richly-typed structures is to eliminate **partial fucntions** from our code base  
> If we never use partial list functions, length-indexed vectors are not particularly useful

## Type Families

``` hs
appendV :: Vec a m -> Vec a n -> Vec a ???
```

We cant to write `m + n` in the `???` above, but we do not have addition defined for kind `Nat`

We can define a normal Haskell function easily though:

``` hs
plus :: Nat -> Nat -> Nat
plus Z y = y
plus (S x) y = S (plus x y)
```

This function is not applicable to **type-level** `Nat`s though; we need a **type-level function**

Type level functions, also called **type-families**, are defined in Haskell like so:

``` hs
{-# LANGUAGE TypeFamilies #-}
type family Plus (x :: Nat) (y :: Nat) :: Nat where
  Plus Z     y = y
  Plus (S x) y = S (Plus x y)
```

We can use our type family to define `appendV`:

``` hs
appendV :: Vec a m -> Vec a n -> Vec a (Plus m n)
appendV Nil         ys = ys
appendV (Cons x xs) ys = Cons x (appendV xs ys)
```

If we had implemented `Plus` by recursing on the second argument instead of the first:

``` hs
{-# LANGUAGE TypeFamilies #-}
type family Plus (x :: Nat) (y :: Nat) :: Nat where
  Plus' x Z     = x
  Plus' x (S y) = S (Plus' x y)
```

The our `appendV` code would not type check:

``` hs
appendV :: Vec a m -> Vec a n -> Vec a (Plus' m n)
appendV Nil         ys = ys
appendV (Cons x xs) ys = Cons x (appendV xs ys)
```

**Why?** Consider the `Nil` case. We know `m = Z` and must show our desired return type `Plus' Z n` equals our given type `n`, but the fact is not immediately apparent from the equations.

Note: this covers only a small part of the full power of type-based specifications
