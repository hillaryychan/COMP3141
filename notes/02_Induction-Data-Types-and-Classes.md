# Induction, Data Types and Classes

## Induction

Suppose we want to prove that a property *P(n)* holds for **all** natural numbers *n*.  
Remember that the set of natural numbers *N* can be defined as follows:

> Definition of Natural Numbers:  
> (1) 0 is a natural number  
> (2) For any natural number n, n+1 is also a natural number

Therefore, to show *P(n)* for all *n*, it suffices to show:

1. *P(0)* (the **base case**), and
2. assuming *P(k)* (the **inductive case**) ⇒ *P(k+1)* (the **inductive case**)

Example:

![induction example](../imgs/02-2_induction-example.jpg)

### Induction on Haskell Lists

Haskell lists can be defined similarly to natural numbers

> Definition of Haskell Lists:  
> (1) `[]` is a list  
> (2) For any list `xs`, `x:xs` is also a list (for any item `x`)

This means, if we want to prove that a property `P(ls)` holds for all lists `ls`, it suffices to show:

1. `P([])` (the base case)
2. `P(x:xs)` for all items `x`, assuming the inductive hypothesis `P(xs)`

Example:

``` hs
sum :: [Int] -> Int
sum []     = 0          -- 1
sum (x:xs) = x + sum xs -- 2

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z                  -- A
foldr f z (x:xs) = x `f` foldr f z xs -- B

-- Prove for all ls:
sum ls == foldr (+) 0 ls
```

![list induction](../imgs/02-5_list-induction.png)

## Data Types

### Custom Data Types

So far, we have seen ***type synonyms*** using the `type` keyword. For a graphics library we might define:

``` hs
type Point  = (Float, Float)
type Vector = (Float, Float)
type Line   = (Point, Point)
type Colour = (Int, Int, Int, Int) -- RGBA

movePoint :: Point -> Vector -> Point
movePoint (x,y) (dx,dy) = (x + dx, y + dy)
```

But these definitions allow `Points` and `Vectors` to be used interchangeable, increasing the likelihood of errors. i.e we would not receive any errors if we passed a `Vector` as a `Point` and vice versa.

We can define out own compound types using the `data` keyword

![custom types](../imgs/02-7_custom-types.png)

Note: the type name and constructor name don't have to be the same  
Note: `deriving (Show, Eq)` means "make this printable and comparable"

#### Records

We could define Colour as a **product type**:

``` hs
data Colour = Colour Int Int Int Int
```

But this has so many parameters, it's hard to tell which is which.

Haskell lets us declare these types as **records**, which is identical to the declaration type on `data`, but also gives us projection functions and record syntax:

``` hs
data Colour = Colour { redC     :: Int
                     , greenC   :: Int
                     , blueC    :: Int
                     , opacityC :: Int
                     } deriving (Show, Eq)
redC (Colour 255 128 0 255) -- gives 255

-- it is equivalent to
data Colour = Colour Int Int Int Int
redC     (Colour r _ _ _) = r
greenC   (Colour _ g _ _) = g
blueC    (Colour _ _ b _) = b
opacityC (Colour _ _ _ o) = o
```

### Patterns in Function Definitions

Patterns are used to deconstruct a value of a particular type.  
A pattern can be a binding to a hole (`_`), a name, or a constructor of the type. When defining a function, each argument is bound using a separate pattern.

``` hs
if' :: Bool -> a -> a -> a
if' True  then' _     = then'
if' False _     else' = else'
```

``` hs
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
```

``` hs
isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _   = False
```

### Enumeration Types

Similar to `enums` in C and Java, we can define types to have one of a set of predefined values:

``` hs
data LineStyle = Solid
               | Dashed
               | Dotted
               deriving (Show, Eq)

data FillStyle = SolidFill | NoFill
               deriving (Show, Eq)
```

Types with more than one constructor are called **sum types**. Constructors are how an value of a particular type is created:

``` hs
data Bool = True | False
data Int = .. | (-1) | 0 | 2 | 3 | ..
data Char = 'a' | 'b' | 'c' | 'd' | 'e' | ..
```

Just as the `Point` constructor took two `Float` arguments, constructors for sum types can take parameters too, allowing us to model different kinds of shape:

``` hs
data PictureObject = Path    [Point]        Colour LineStyle
                   | Cirlce  Point Float    Colour LineStyle FillStyle
                   | Polygon [Point]        Colour LineStyle FillStyle
                   | Ellipse Point Float Float Float
                             Colour LineStule FillStyle
                   deriving (Show, Eq)

type Picture = [PictureObject]
```

Here, type creates a ***type alias*** which provides only an alternate name which refers to an existing type.

### Recursive and Parametric Types

Data types can also be defined with ***parameters***, such as the well known `Maybe` type, defined in the standard library.

``` hs
data Maybe a = Just a | Nothing
```

Types can also be ***recursive***. If lists weren't already defined in the standard library, we could define them ourselves:

``` hs
data List a  = Nil | Cons a (list a)
```

We can even define natural numbers, where 2 is encoded as `Succ (Succ Zero)`:

``` hs
data Nat= Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ a) b = add a (Succ b)  -- (a + 1) + b == a + (b + 1)

zero = Zero
one = Succ Zero
two = add one one

-- Nat is recursive as it has the (Succ) constructor which takes a Nat
-- Nat has the Zero constructor which does not recurse and acts like a base case
```

### Types in Design

Advice from Yaron Minsky (of Jane Street): **Make illegal states ***unrepresentable***.**

Choose types that *constrain* your implementation as much as possible. Then failure scenarios are eliminated automatically.

Example: We eliminate the failure state of no contact details

``` hs
data Contact = C Name (Maybe Address) (Maybe Email)
-- is changed to
data ContactDetails = EmailOnly Email
                    | PostOnly Address
                    | Both Address Email
data Contact = C Name ContactDetails
```

### Partial Functions

Failure to follow Yaron's advice leads to **partial functions**.

A **partial function** is a function that is not defined for all possible inputs.  
Examples: `head`, `tail`, `(!!)`, division

Partial functions are to be avoided, because they cause your program to crash if undefined cases are encountered.

To eliminate partiality, we must either:

* **enlarge** the codomain, usually with a `Maybe` type

    ``` hs
    safeHead :: [a] -> Maybe a
    safeHead (x:xs) = Just x
    safeHead []     = Nothing
    ```

* or we must **constrain** the domain to be more specific:

    ``` hs
    safeHead' :: NonEmpty a -> a
    ```

## Type Classes

We've seen functions that work on multiple types (e.g. `compare`, `(==)`, `(+)`, `show`) and their corresponding constraints on type variables (e.g `Ord`, `Eq`, `Num` and `Show`).  
These constraints are called **type classes**, and can be thought of as a *set of types* for which certain operations are implemented.

Type classes describe a set of behaviours that can be implemented for any type. A function or type class inheritance (i.e `deriving (Show)`) can operate on a type variable constrained by a type class instead of a concrete type. It is similar to an OOP interface

When creating an instance of a type class with ***laws***, you must ensure that the laws are held manually (they cannot be checked by the compiler).  
When using a type class with ***laws***, you can assume that all laws hold for all instances of the type class.

### Show

The `Show` type class is a set of types that can be converted to strings. It is defined like:

``` hs
class Show a where -- class is not related to OOP
  show :: a -> String

-- Types are added to the type class as an instance like so
instance Show Bool where
  show True  = "True"
  show False = "False"

-- We can also define instances that depend on other instances
instance Show a => Show (Maybe a) where
  show (Just x) = "Just " ++ show x
  show Nothing  = "Nothing"
```

Haskell supports automatically `deriving` instances for some classes, including `Show`

### Read

Type classes can also ***overload*** based on the type returned, unlike similar features like Java's interfaces. `Read` allows us to take a string representation of a value and decode it.

``` hs
class Read a where
  read :: String -> a

-- Examples:
read "34" :: Int           -- give 34 as an integer
read "22" :: Char          -- gives a runtime error
show (read "34") :: String -- gives a type error since show expects any time
                           --   ghci will convert it to a default value
                           --   but it is often not what we expect
```

### Semigroup

A **semigroup** is a pair of a set S and an operation `• : S → S → S`, where the operation • is **associative**.  
Associativity is defined as, for all a, b, c: `(a • (b • c)) = ((a • b) • c)`

Haskell has a type class for semigroups.  
The associativity law is enforced only by programmer discipline

``` hs
class Semigroup s where
  (<>) :: s -> s -> s
  -- Law: (<>) must be associative
```

Some associative operators include: `(++)`, `(+)`, `(*)`

Let's implement additive colour mixing:

``` hs
instance Semigroup Colour where
  Colour r1 g1 b1 a1 <> Colour r2 g2 b2 a2
    = Colour (mix r1 r2)
             (mix g1 g2)
             (mix b1 b2)
             (mix a1 a2)
    where mix x1 x2 = min 255 (x1 + x2)
-- observe that associativity is satisfied
```

#### Monoid

A **monoid** is a semigroup (S, •) equipped with a special ***identity element*** `z :: S` such that `x • z = x` and `z • y = y` for all x, y

``` hs
class (Semigroup a) => Monoid a where
  mempty :: a
```

For colours, the identity element is transparent black:

``` hs
instance Monoid Colour where
  mempty = Colour 0 0 0 0
```

There are multiple possible monoid instances for numeric types like `Integer`:

* The operation `(+)` is associative, with identity element 0
* The operation `(*)` is associative, with identity element 1

Haskell doesn't use any of these, because there can only be **one** instance per type per class in the ***entire program*** (including all dependencies and libraries used)

##### Newtypes

A common technique is to define a ***separate type*** that is represented identically to the original type, but can have its own, different type class instances.  
`newtype` allows you to encapsulate an existing type to add constraints or properties without adding runtime overload.  
In Haskell, this is done with the `newtype` keyword

A `newtype` declaration is much like a `data` declaration except that there can be only **one constructor** and it must take exactly **one argument**:

``` hs
newtype Score = S Integer

instance Semigroup Score where
  S x <> S y = S (x + y)

instance Monoid Score where
  mempty = S 0
```

Here `Score` is represented identically to `Integer`, and thus no performance penalty is incurred to convert between them.  

Example with kilometres and miles:

``` hs
newtype Kilometers = Kilometers Float
newtype Miles = Miles Float

kilometresToMiles :: Kilometers -> Miles
kilometresToMiles (Kilometers kms) = Miles $ kms / 1.60934

milesToKilometers :: Miles -> Kilometers
milesToKilometers (Miles miles) = Kilometers % miles * 1.60934
```

***In general, `newtypes` are a great way to prevent mistakes***. Use them frequently

### Ord

`Ord` allows us to compare two values of a type for **partial** or **total inequality**

``` hs
class Order a where
  (<=) :: a -> a -> Bool
```

Instances should satisfy the following laws:

1. **Reflexivity**: `x <= x`
2. **Transitivity**: If `x <= y` and `y <= x`, then `x <= z`
3. **Antisymmetry**: If `x <= y` and `y <= x` then `x == y`
4. **Totality**: Either `x <= y` or `y <= x`

Relations that satisfy these four properties are called **total orders**.  
Without the fourth (totality), they are called **partial orders**. An example of this would be `(/)` (division). Two numbers don't necessarily divide each other

### Eq

`Eq` allows us to compare two values of a type for an **equivalence** or **equality**;

``` hs
class Eq a where
  (==) :: a -> a -> Bool
```

Instances should satisfy the following laws:

1. **Reflexivity**: `x == x`
2. **Transitivity**: If `x == y` and `y == z`, then `x == z`
3. **Symmetry**: If `x == y` then `y == x`
4. **Negation** (equality): If `x =/= y` then `¬(x = y)`
5. **Substitutivity** (equality): If `x == y` then `f x == f y` for all functions `f`

Relations that satisfy **laws 1-3** are called **equivalence relations**  
Relations that satisfy **laws 1-5** are called **equality relations**

## Functors

Haskell is actually comprised of ***two languages***  
The **value-level/runtime** language, consisting of expressions such as `if`, `let`, `3` etc.  
The **type-level** language, consisting of types `Int`, `Bool`, synonyms like `String`, and type ***constructors*** like `Maybe`, `(->)`, `[  ]` etc.

This type level language itself has a type system!

Just as values and functions in the *runtime language* has types, types in the *type language* of Haskell have **kinds**.  
The most basic kind is written as `*`.

Seeing as `Maybe` is parameterised by one argument, `Maybe` has kind `* -> *`  
`Maybe` is a type constructor that takes a type and produces a type that may or may not hold a value  
`Maybe Int` is a concrete type that may or may not hold an `Int`

### Lists

Suppose we have a function:

``` hs
toString :: Int -> String
```

And we also have a function to give us some numbers:

``` hs
getNumbers :: Seed -> [Int]
```

We can compose `toString` with `getNumbers` to get a function `f :: Seed -> [String]`

``` hs
f = map toString . getNumbers
```

### Maybe

Suppose we have a function:

``` hs
toString :: Int -> String
```

And we also have a function that ***may*** give us a number:

``` hs
tryNumber :: Seed -> Maybe Int
```

We can compose `toString` with `tryNumber` to get a function `f :: Seed -> Maybe String`  

``` hs
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing  = Nothing
maybeMap f (Just x) = Just (f x)
-- alternatively it can be written as
maybeMap f mx = case mx of
                  Nothing -> Nothing
                  Just x  -> Just (f x)

f = maybeMap toString . tryNumber
```

### Functors Explained

All these functions (i.e `maybeMap`, `map`) are in the interface of a single type class called `Functor`

``` hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Unlike previous type classes we've seen like `Ord` and `Semigroup`, `Functor` is over types of kind `* -> *`

Instances for:

* lists
* `Maybe`
* Tuples

    ``` hs
    -- type level:
    -- (,) :: * -> (* -> *)
    -- (,) x :: * -> *

    instance Functor ((,) x) where
    --  fmap :: (a -> b) -> f a -> f b
    --  fmap :: (a -> b) -> (,) x a -> (,) x b
    --  fmap :: (a -> b) -> (x, a) -> (x, b)
      fmap f (x,a) = (x, f a)
    -- the functor instance for tuples just applies
    -- the function to the right-hand side of the tuple
    ```

* Functions

    ``` hs
    -- type level:
    -- (->) :: * -> (* -> *)
    -- (->) x :: * -> *

    instance Functor ((->) x) where
    --  fmap :: (a -> b) -> f a -> f b
    --  fmap :: (a -> b) -> (->) x a -> (->) x b
    --  fmap :: (a -> b) -> (x -> a) -> (x -> b)
      fmap = (.)
    -- the functor instance of (->) simply composes functions
    ```

You can think of a functor as a way to define behaviour of a type when given to `fmap`:

1. Define a type (e.g. `Type_from_step_1`) that is able to be mapped over
2. Use `instance Functor Type_from_step_1 where ...` to define the behaviour that should occur when we call `fmap` on a variable with the given `Type_from_step_1`.

Note: `type_from_step_1` must take a type parameter. For example, `Maybe` takes a type parameter (`Int`, `Bool`, `Char` etc.) to become `Maybe Int`, `Maybe Bool`, `Maybe Char` etc. This is what is meant by "kinds".

### Functor Laws

The functor type class must obey two laws:

> (1) Idenity Law: `fmap id == id`  
> E.g. if the identity function is applied to every element in a structure (e.g. a list) we'd get the same structure back  
> (2) Composition Law: `fmap f . fmap g == fmap (f . g)`  
> E.g. if we apply g on every element in a structure (e.g. a list), the we apply g on every element. this should be equivalent to applying (f . g) on every element on the structure

In Haskell's type system, it's impossible to make a total `fmap` function that satisfies the first law but violated the second.  
In other words, ***if one law is satisfied, then the other law is also satisfied***.

This is due to [parametricity](TODO)
