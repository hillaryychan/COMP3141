# Functional Programming

When developing software, we have a trade-off:  
Software must be high quality; **correct, safe, secure**  
Software must be developed **cheaply and quickly**

We have **safety-uncritical** applications like video games, where it is acceptable to have bugs in order to save developer effort.

We also have **safety-critical** applications; e.g. planes, self-driving cars, radiation therapy machines etc.

In this course we will use mathematics to guide the development of software

![overview](../imgs/01-9_overview.jpg)

## Haskell

Haskell is a language with good support for mathematically structured programming.  
It simply takes in an input and produces a result.

![Haskell structure](../imgs/01-15_haskell-structure.png)

In mathematics, we would apply a function by writing *f(x)*. In Haskell we write `f x`.

Features of the Haskell language:

* Immutable data - no/less side-effects
* Declarative - specifies **what** is to be done instead of **how** to do it
* Easier to verify - can mathematically prove the correctness of programs
* Lazy evaluated - it evaluates things as they are needed e.g.  

    ``` hs
    func arg =
      let x = func1 arg         -- evaluate z,
          y = func2 arg         -- if z is True,
          z = func3 arg         -- evaluate x,
      in if z then x else y     -- otherwise evaluate y
    ```

### Currying

In mathematics, we treat log10(x) and log2(x) and ln(x) as separate functions  
In Haskell, we have a single `logBase` that, given a number *n*, produces a function for logn(x)

``` hs
log10 :: Double -> Double
log10 = logBase 10

log2 :: Double -> Double
log2 = logBase 2

ln :: Double -> Double
ln = logBase 2.71828
```

The type of `logBase` is `logBase :: Double -> (Double -> Double)`, where parentheses are optional.

Function application associates to the **left** in Haskell, so  
logBase 2 64 ≡ (logBase 2) 64

Functions of more than one argument are usually written this way in Haskell, but it is possible to use **tuples** instead

### Tuples

**Tuples** are another way to take multiple inputs or produce multiple outputs:

``` hs
toCartesian :: (Double, Double) -> (Double, Double)
toCartesian (r, theta) = (x, y)
  where x = r * cos theta
        y = r * sin theta
```

Note that the order of bindings doesn't matter. Haskell functions have no side effects, they just return result.

### Higher Order Functions

A **higher order function** is a function which takes another functions an argument

``` hs
twice :: (a -> a) -> (a -> a)
twice f a = f (f  a)

double :: Int -> Int
double x = x * 2

quadruple :: Int -> Int
quadruple = twice double

-- twice twice double 3 produces 48
-- (twice twice double) 3
-- (twice (twice double)) 3
-- (twice quadruple) 3
```

### Strings

The type `String` in Haskell is just a list of characters

``` hs
type String = [Char]
```

This is a **type synonym**, like a `typedef` in C.  
Thus

``` hs
"hi!" == ['h', 'i', '!']
```

### Lists

Haskell makes extensive use of lists, constructed using square brackets. Each list element **must** be of the **same type**

``` hs
[True, False, True] :: [Bool]
[3, 2, 5+1] :: [Int]
[sin, cos] :: [Double -> Double]
[ (3,'a'), (4,'b') ] :: [(Int, Char)]
```

Haskell lists are *singly-linked* lists. Lists can be constructed by so-called constructors. There are two of them:

* `[]` - the empty list
* `x:xs` - the ***colon*** (or ***prepend*** or ***cons***). The `:` prepends any element `x` to an already existing list `xs`  
The value `x` is called the **head** and the rest of the list `xs` is called the **tail**. Thus

    ``` hs
    "hi!" == ['h', 'i', '!'] == 'h':('i':('!':[]))
                             == 'h' : 'i' : '!' : []
    ```

Functions on lists can be imported via `import Data.List`

When we define recursive functions on lists, we use the fast form for pattern matching:

``` hs
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

### Map

`map` is a useful function, which given a function, applies it to each element of a list

``` hs
map not [True, False, True] = [False, True, False]
map negate [3, -2, 4]       = [-3, 2, -4]
map (\x -> x + 1) [1, 2, 3] = [2, 3, 4]
```

The last example uses a ***anonymous functions*** to define a one-use function without giving it a name. The template for anonymous functions is: `(\<args> -> <expr>)`

The type of `map` is `map :: (a -> b) -> [a] -> [b]`

We can evaluate programs *equationally*

``` hs
map toUpper "hi!" ≡ map toUpper (’h’:"i!")
                  ≡ toUpper ’h’ : map toUpper "i!"
                  ≡ ’H’ : map toUpper "i!"
                  ≡ ’H’ : map toUpper (’i’:"!")
                  ≡ ’H’ : toUpper ’i’ : map toUpper "!"
                  ≡ ’H’ : ’I’ : map toUpper "!"
                  ≡ ’H’ : ’I’ : map toUpper (’!’:"")
                  ≡ ’H’ : ’I’ : ’!’ : map toUpper ""
                  ≡ ’H’ : ’I’ : ’!’ : map toUpper []
                  ≡ ’H’ : ’I’ : ’!’ : []
                  ≡ "HI!"                                       6
```

### Function Composition

We use **function composition** to combine our functions together. The mathematical (f ◦ g )(x) is written `(f . g) x` in Haskell.

In Haskell, operators like function composition are themselves functions. You can define your own operators

``` hs
-- Vector addition
(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

(2,3) .+ (1,1) == (3,4)
```

You could even have defined function composition yourself if it didn't already exist:

``` hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = f (g x)
-- f . g equiv. to (\x -> f (g x))
```

### Native Higher Order Functions

Various list functions that are built into Haskell's standard library

* `map`
* `filter`
* `concat`
* `sum`
* `foldr`
* `foldl`

### Miscellaneous Syntax

#### `let`

#### `case`

#### `if`

#### Guards

``` hs
fac n =
  if n <= 1 then
    1
  else
    n * fac (n-1)

fac c
| n <= 1    = 1
| otherwise = n * fac (n-1)

Guards need boolean expressions
otherwise is a constant that always evaluates to True
```

#### Dollar Sign

The `$` operator is an infix operator, which given a `a -> b` function and an `a` to apply it to, it gives us b.

``` hs
($) :: (a -> b) -> a -> b

-- the following are equivalent
f xs = map (\x -> x+1) (filter (\x -> x>1) xs)
f xs = map (\x -> x+1) $ filter (\x -> x>1) xs
```
