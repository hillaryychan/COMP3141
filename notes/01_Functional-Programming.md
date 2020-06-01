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

![haskell structure](../imgs/01-15_haskell-structure.png)

In mathematics, we would apply a function by writing *f(x)*. In Haskell we write `f x`.

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

Tuples are another way to take multiple inputs or produce multiple outputs:

``` hs
toCartesian :: (Double, Double) -> (Double, Double)
toCartesian (r, theta) = (x, y)
  where x = r * cos theta
        y = r * sin theta
```

Note that the order of bindings doesn't matter. Haskell functions have no side effects, they just return result.

### Higher Order Functions

In addition to returning functions, functions can take other functions as arguments

``` hs
twice :: (a -> a) -> (a -> a)
twice f a = f (f  a)

double :: Int -> Int
double x = x * 2

quadruple :: Int -> Int
quadruple = twice double

-- twice twice double 3 produces 48
-- (twice twice double) 3
-- (twice (twice double) 3
-- (twice quadruple) 3
```

### Lists

Haskell makes extensive use of lists, constructed using square brackets. Each list element **must** be of the **same type**

``` hs
[True, False, True] :: [Bool]
[3, 2, 5+1] :: [Int]
[sin, cos] :: [Double -> Double]
[ (3,'a'), (4,'b') ] :: [(Int, Char)]
```

### Map

`map` is a useful function, which given a function, applies it to each element of a list

``` hs
map not [True, False, True] = [False, True, False]
map negate [3, -2, 4]       = [-3, 2, -4]
map (\x -> x + 1) [1, 2, 3] = [2, 3, 4]
```

The last example uses a ***lambda expression*** to define a one-use function without giving it a name

The type of `map` is `map :: (a -> b) -> [a] -> [b]`
