# Functors, Applicatives, and Monads

These are common abstractions used in functional programming and, increasingly, in imperative programming as well.  
These abstractions are made concrete into actual type classes in Haskell, where they are often left as mere "design patterns" in other programming languages.

## Recap

### Kinds

Recall that terms in the type level language of Haskell are given **kinds**. The most basic kind written as `*`.  
Types such as `Int` and `Bool` have kind `*`  
Seeing as `Maybe` is parameterised by one argument, `Maybe` has kind `* -> *`: given a type (e.g. `Int`) it will return a type `Maybe Int`.  
The kind of `State` is `* -> * -> *` for its state, return value, and ultimate return value.

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

    ``` hs
    ioMap :: (a -> b) -> IO a -> IO b
    ioMap f act = do
      a <- act
      pure (f a)
    ```

* `State s`

    ``` hs
    stateMap :: (a -> b) -> State a -> State b
    stateMap f act = do
      a <- act
      pure (f a)
    ```

* `Gen` - recall `Gen a` is a random generator of values of type `a`

    ``` hs
    sortedLists :: (Arbitrary a, Ord a) => Gen [a]
    sortedLists = fmap sort (listOf arbitrary)
    -- listOf :: Gen a -> Gen [a]
    ```

    ``` hs
    monadMap :: Monad m => (a -> b) -> m a -> m b
    monadMap f act = do
      a <- act
      pure (f a)
    ```

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

Suppose we want to lookup a student's zID and program code using these functions:

``` hs
lookupID :: Name -> Maybe ZID
lookupProgram :: Name -> Maybe Program
```

and we had a function

``` hs
makeRecord :: ZID -> Program -> StudentRecord
```

We can combine these functions to get a function of type `Name -> Maybe StudentRecord`.  
We need a function to take:

* a `Maybe`-wrapped function `Maybe (Program -> StudentRecord)`
* a `Maybe`-wrapped argument `Maybe Program`

And apply the function to the argument, giving us a result of type `Maybe StudentRecord`

This is encapsulated by a subclass of `Functor` called `Applicative`:

``` hs
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Applicatives allow us to *"wrap"* functions just like how we can *"wrap"* values in `Functor`

`Maybe` is an instance, so we can use this for `lookupRecord`

``` hs
lookupRecord' :: Name -> Maybe StudentRecord
lookupRecord' n = let zid     = lookupID n
                      program = lookupProgram n
                  in fmap makeRecord zid <*> program
               -- or pure makeRecord <*> zid <*> program
```

In general, we can take a regular function application: `f a b c d`  
and apply that function to `Maybe` (or other `Applicative`) arguments using this pattern (where `<*>` is left-associative)  

``` hs
pure f <*> ma <*> mb <*> mc <*> md
```

All law-abiding instances of `Applicative` are also instance of `Functor`, by defining:

``` hs
fmap f x = pure f <*> x
```

Sometimes this is written as an infix operator, `<$>`, which allows us to write:

``` hs
pure f <*> ma <*> mb <*> mc <*> md
-- as
f <$> ma <*> mb <*> mc <*> md
```

So:

``` hs
fmap f x == pure f <*> x
         == f <$> x
```

### Applicative Laws

There are 4 applicative laws:  
These laws are not expected to be memorised, but should be paid attention to when defining instances.

``` hs
-- 1. Identity
pure id <*> v = v

-- 2. Homomorphism
pure f <*> pure x = pure (f x)

-- 3. Interchange
u <*> pure y = pure ($ y) <*> u

-- 4. Composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

Proving the Functor Laws hold if we implement `fmap` as `pure f <*> x`

``` hs
-- Functor laws:
-- 1. fmap id x = x
-- 2. fmap f (fmap g x) = fmap (f . g) x

-- Proof:
-- 1) pure id <*> x == x -- true by Identity law
--
-- 2) pure f <*> (pure g <*> x)
--      == pure (.) <*> pure f <*> pure g <*> x -- Composition
--      == pure ((.) f) <*> pure g <*> x        -- Homomorphism
--      == pure (f.g) <*> x                     -- Homomorphism
```

### Applicative Instances

#### Applicative Lists

There are **two** ways to implement `Applicative` for lists:

``` hs
(<*>) :: [a -> b] -> [a] -> [b]
```

1. Apply each of the given functions to each of the arguments, concatenating all the results
2. Apply each function in the list of functions to the corresponding value in the list of arguments. (This one is put in a `newtype` `ZipList` in the Haskell standard library)

``` hs
pureZ :: a -> [a]
pureZ a = a : pureZ a

pureC :: a -> [a]
pureC a = [a]

applyListZ :: [a -> b] -> [a] -> [b]
applyListZ (f:fs) (x:xs) = f x ++ applyListZ fs xs
applyListZ [] _ = []
applyListZ _ [] = []

applyListC :: [a -> b] -> [a] -> [b]
applyListC (f : fs) args = map f args ++ map fs args
applyListC [] args = []
```

#### Other Instances

* `Maybe`

    ``` hs
    instance Applicative Maybe where
      pure x =  Just x
      Just f <*> Just x = Just (f x)
      _      <*> _      = Nothing
    ```

* Lists

    ``` hs
    instance Applicative [ ] where
      pure x = [x]
      [] <*> ys = []
      (f:fs) <*> xs = map f xs ++ (fs <*> xs)
    ```

* QuickCheck generators: `Gen`  

    ``` hs
    data Concrete = C [Char] [Char] deriving (Show, Eq)

    instance Arbitrary Concrete where
      arbitrary = C <$> arbitrary <*> arbitrary
    ```

* Functions: `((->) x)`

    ``` hs
    instance Applicative ((->) x) where
      pure :: a -> x -> a
      pure a x = a

   -- (<*>) :: (->) x (a -> b) -> (->) x a -> (->) x b
   -- which is equivalent to
      (<*>) :: (x -> a -> b) -> (x -> a) -> (x -> b)
      (<*>) xab xa x = xab x (xa x)
    ```

* Tuples: `((,) x)` -  it is necessary for `x` to be a `Monoid`

    ``` hs
    instance Monoid x => Applicative ((,) x) where
      pure :: a -> (x, a)
      pure a = (mempty , a)

      (<*>) :: (x,a -> b) -> (x, a) -> (x, b)
      (<*>)(x, f) (x', a) = (x <> x' , f a)
    ```

* `IO` and `State s`

    ``` hs
    instance Applicative IO where
    pure :: a -> IO a
    pure a = pure a

    (<*>) :: IO (a->b) -> IO a -> IO b
    pf (<*>) pa = do
      f <- pf
      a <- pa
      pure (f a)
    ```

## Monads

**Functors** are types for containers where we can `map` pure functions on their contents.  
**Applicative Functors** are types where we can combine *n* containers with a *n*-ary function.

The last and most commonly-used higher-kinded abstraction in Haskell programming is the `Monad`. **Monads** are type `m`, where we can ***sequentially compose*** functions of the form `a -> m -> b`

``` hs
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
```

Sometimes in old documentation the function `return` is included here, but it is just an alias for `pure`. It has nothing to do with `return` as in C/Java/Python.

Consider:

* `Maybe`

    ``` hs
    instance Monad Maybe where
      Just x >>= f  = f x
      Nothing >>= f = Nothing
    ```

* Lists

    ``` hs
    instance Monad [ ] where
      xs >>= each = concatMap each xs
    ```

* `(x ->)` (the **Reader** monad)

    ``` hs
    instance Monad ((->) x) where
      (xa >>= axb) = \x -> axb (xa x) x
    ```

* `(x,)` (the **Write** monad, assuming `Monoid` instance for `x`)
* `Gen`
* `IO, State s` etc.

We can define a composition operator with `(>>=)`:

``` hs
(<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)
(f <=< g) x = g x >>= f
```

**Monad Laws**:

``` hs
f <=< (g <=< x) == (f <=< g) <=< x -- associativity
pure <=< f      == f               -- left identity
f <=< pure      == f               -- right identity
```

These are similar to the monoid laws, generalised for multiple types inside the monad.  
This sort of structure is called a **category** in mathematics.

All `Monad` instances give rise to an `Applicative` instance, because we can define `<*>` in terms of `>>=`.

``` hs
mf <*> mx = mf >>= \f -> mx >>= \x -> pure (f x)
```

This implementation is already provided for `Monads` as the `ap` function in `Control.Monad`

### `do` Notation

Working directly with monad functions can be unpleasant, so Haskell has some notation to increase niceness:

``` hs
do x <- y
   z
-- becomes
y >>= \x -> do z

do x
   y
-- becomes
x >>= \_ -> do y
```

### Examples

#### Dice Rolls

Roll two 6-sided dice, if the difference is < 2, re-roll the second die. Final score is the difference of the two die. What score is most common?

``` hs
roll :: [Int]
roll = [1,2,3,4,5,6]

diceGame = do
  d1 <- roll
  d2 <- roll
  if (abs (d1 - d2) < 2) then do
    d2' <- roll
    pure (abs (d1 - d2'))
  else
    pure (abs (d1 - d2))

map length $ group sort diceGame
-- gives [16,28,30,22,14,6] meaning 2 is the most common score
```

#### Partial Functions

We have a list of student names in a database of type `[(ZID, Name)]`. Given a list of zID’s, return a `Maybe [Name]`, where `Nothing` indicates that a zID could not be found.

``` hs
db :: [(ZID, Name)]
db = [(3253158, "Liam")
      (8888888, "Rich")
      (4444444, "Mort")]

studentNames :: [ZID] -> Maybe [Name]
studentNames [] = pure []
studentNames (z:zs) = do
  n <- lookup z db
  ns <- studentNames zs
  pure (n:ns)

-- can also be written as
studentNames (z:zs) = (:) <$> lookup z db <*> studentNames zs
```

#### Arbitrary Instances

Define a Tree type and a generator for search trees:

``` hs
searchTrees :: Int -> Int -> Generator Tree
```

``` hs
data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

instance Arbitrary (Tree Int) where
  arbitrary = do
    mn <- (arbitrary :: Gen Int)
    Positive delta <- arbitrary
    let mx = mn + delta
    searchTree mn mx
  where
    searchTree :: Int -> Int -> Gen (Tree Int)
    searchTree mn mx | mn >= mx   = pure Leaf
                     | otherwise = do
                           v <- choose (mn,mx)
                           l <- searchTree mn v
                           r <- searchTree (v+1) mx
                           pure (Branch v l r)
```

## Resources

Stuff that helped me understand Functors, Applicatives and Monads:

* [Functors](https://youtu.be/xCut-QT2cpI)
* [Functor Applicatives](https://youtu.be/CNOff5LPKQI)
* [Monads](https://youtu.be/f1Y7vLakykk)
