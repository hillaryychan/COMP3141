# Functors, Applicatives, and Monads

These are common abstractions used in functional programming and, increasingly, in imperative programming as well.  
These abstractions are made concrete into genuine type classes in Haskell, where they are often left as mere "design patterns" in other programming languages.

## Recap

### Kinds

Recall that terms in the type level language of Haskell are given **kinds**. The most basic kind written as `*`.  
Types such as `Int` and `Bool` have kind `*`  
Seeing as `Maybe` is parameterised by one argument, `Maybe` has kind `* -> *`: given a type (e.g. `Int`) it will return a type `Maybe Int`.

### Functors

Recall the type class defined over type constructors is called **`Functor`**

``` hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

> **Functor Laws**
>
> 1. `fmap id == id`
> 2. `fmap f . fmap g = fmap (f . g)`

We've seen instances for lists, `Maybe`, tuples and functions.  
Other instances include:

* `IO`
* `State s`
* `Gen`

### QuickCheck Generators

Recall the `Arbitrary` class has a function

``` hs
arbitrary :: Gen a
```

This type `Gen` is an **abstract type** for QuickCheck generators. Suppose we have a function:

``` hs
toString :: Int -> String
```

And we want a generator for `String` (i.e `Gen String`) that is the result of applying `toString` to arbitrary `Int`s, then ***we use `fmap`***

## Applicative Functors
