# Property Based Testing, Lazy Evaluation

## Free Properties

Haskell already ensures certain properties automatically with its language design and type system

1. Memory is accessed where and when it is safe and permitted to be accessed (**memory safety**)
2. Values of a certain static type will actually have that type at run time
3. Programs that are well-typed will not lead to undefined behaviour (**type safety**)  
i.e if a function of type `Int -> Int`, when given an `Int` it can only return an `Int`
4. All functions are **pure**: Programs won't have side effects not declared in the type (**purely functional programming**)

Most of our properties focus on the ***logic of our program***

## Logical Properties

We have already seen a few properties of logical properties.

> Example (Properties)
>
> 1. `reverse` is an *involution*: `reverse (reverse xs) == xs`
> 2. right identity for `(++): xs ++ [] == xs`
> 3. transitivity of `(>): (a > b) ^ (b > c) ⇒ (a > c)`

The set of properties that capture all of our requirements for our program is called the **functional correctness specification** of our software.

This defines what it means for software to be *correct*.

## Proofs

Last week we saw some *proof methods* for Haskell programs. We could *prove* that our implementation meets its functional correctness specification.

Such proofs certainly offer a high degree of assurance, but:

* Proofs must make some ***assumptions*** about the environment and the semantics of the software
* ***Proof complexity grows with implementation complexity***, sometimes drastically
* If software is *incorrect*, a proof attempt might simply become stuck: we do not always get constructive negative feedback
* Proofs can be labour and time intensive ($$$), or require highly specialised knowledge ($$$)

## Testing

Compared to proofs:

* Tests typically run the actual program, so it requires ***fewer assumptions*** about the language semantics or operating environment
* Test ***complexity does not grow with implementation complexity***, so long as the specification is unchanged
* Incorrect software when tested leads to ***immediate, debuggable counterexamples***
* Testing is typically ***cheaper and faster*** than proving
* Tests care about ***efficiency*** and ***computability***, unlike proofs

We *lose* some assurance, but *gain* some convenience ($$$)

## Property Based Testing

The key idea of **property-based Testing (PBT)** is to generate random input values, and test properties by running them

``` hs
prop_reverseApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs
```

Haskell's QuickCheck is the first library ever invented for PBT. The concept has since been ported to Erlang, Scheme, Common Lisp, Perl, Python, Ruby, Java, Scala, F#, OCaml, Standard ML, C and C+.

PBT vs. Unit Testing:

* **Less testing code** - properties are more compact than unit tests, and describe more test cases
* PBT heavily depends on **test data generation**:
    * Random inputs may not be as informative as hand-crafted inputs ⇒ use ***shrinking***
    * Random inputs may not cover all necessary corner cases ⇒ use a ***coverage checker***
    * Random inputs must be generated for user-defined types ⇒ QuickCheck includes functions to build ***custom generators***
* By increasing the number of random inputs, we improve the code coverage in PBT

### Test Data Generation

Data which can be generated randomly is represented by the following type class:

``` hs
class Arbitrary a where
  arbitrary :: Gen a -- more on this later
  shrink :: a -> [a]
```

Most of the types we have seen so far implement `Arbitrary`

The `shrink` function is for when test cases fail. If a given input `x` fails, QuickCheck will try all inputs in `shrink x`, repeating the process until the smallest possible input is found.

### Testable Types

The type of the `quickCheck` function is:

``` hs
quickCheck :: (Testable a) => a -> IO ()
```

The `Testable` type class is the class of things that can be converted into properties.  
This include:

* `Bool` values
* QuickCheck's built-in `Property` type
* Any function from an `Arbitrary` input to a `Testable` output:

    ``` hs
    instance (Arbitrary i, Testable o) => Testable (i -> o) ...
    ```

Thus the type `[Int] -> [Int] -> Bool` (as used in `[prop_reverseApp`](../examples/week3/basic-quickcheck.hs)) is `Testable`

Let's look at an example. Is the function `divisible` reflexive (i.e `x % x == 0`)?

``` hs
divisible :: Integer -> Integer -> Bool
divisible x y = x `mod` y == 0

prop_refl :: Integer -> Bool  -- by convention we prefix property-based tests with prop_
prop_refl x = divisible x x
```

We can **encode pre-conditions** with the `(==>)` operator

``` hs
prop_refl :: Integer -> Property
prop_refl x = x > 0 ==> divisible x x
```

Or, we can **select different gerators** with modifier `newtypes`

``` hs
prop_refl :: Positive Integer -> Bool
prop_refl (Positive x) = divisible x x
```

See [merge sort example](../examples/week3/merge-sort.hs).

### Redundant Properties

Some properties are technically *redundant* (i.e implied by other properties in the specification), but there is some value in testing them anyway.

* They may be **more efficient** than functional correctness test, consuming less computing resources to test
* They may be more **fine-grained** to give better test coverage than random inputs for full functional correctness tests
* They provide good **sanity check** to the full functional correctness properties
* Sometimes full functional correctness is **not easily computable** but tests of weaker properties are.

These redundant properties include **unit tests**. We can (and should) combine both approaches.

### Test Quality

How good are your tests?

* Have you checked that every special case works correctly?
* Is all code exercised in the tests?
* Even if all code is exercised, is it exercised in all contexts?

**Coverage checkers** are useful tools to partially quantify this.

Types of coverage:

* **Entry/Exit Coverage** - All function *calls* executed? Has every defined function been tested?
* **Function Coverage** - All *functions* executed? If a function is called in several places, are all those places tested?
* **Statement/Expression Coverage** - All *statements/expressions* executed? Do we execute each component of code?
* **Branch/Decision Coverage** - All *conditional branches* executed?
* **Path Coverage** - All possible *routes* executed? Have exhaustively analysed every possible behaviour of the program. Extremely difficult to compute.

**Haskell Program Coverage** (or `hpc`) is a GHC-bundled tool to measure function, branch and expression coverage.

To use it:

1. Build with `ghc -fhpc --make prog.hs`
2. Run your program `./prog`.  
This will generate a `prog.tix` file
3. Convert `prog.txt` to a readable format with
    1. `hpc report prog.tix` for a textual report about program coverage
    2. `hpc markup prog.tix` for Markup Haskell source with program coverage

## Lazy Evaluation

The following program crashes when given a large number due to **stack overflow** (the stack pointer exceeds the stack bound).

``` hs
sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = sumTo (n-1) + n
```

The following program now uses an accumulator value but *still* crashes when given a large number due to **space leak** (when the program uses more memory than necessary for computation).

``` hs
sumTo' :: Integer -> Integer -> Integer
sumTo' a 0 = a
sumTo' a n = sumTo' (a+n) (n-1)
```

This is one of the main drawbacks of Haskell's ***lazy evaluation*** method.

Haskell is **lazy evaluated**, also called **call-by-need**.  
This means that expressions are only evaluated when they are ***needed*** to compute a result for the user.

We can force the previous program to evaluate its accumulator by using a **bang pattern**, or the primitive operation `seq`

``` hs
sumTo' :: Integer -> Integer -> Integer
sumTo' !a 0 = a
sumTo' !a n = sumTo' (a+n) (n-1)
-- or
sumTo' :: Integer -> Integer -> Integer
sumTo' a 0 = a
sumTo' a n = let a' = a + n
             in a' `seq` sumTo' a' (n-1)
```

Lazy evaluation has many advantages:

* It enables **equational reasoning** even in the presence of partial functions and non-termination
* It allows functions to be **decomposed** without sacrificing efficiency, for example: `minimum = head . sort` is depends on the sorting algorithm, possibly *O(n)*.  
John Hughes demonstrates αβ pruning from AI as a larger example[^1]
* It allows for **circular programming** and **infinite data structures**, which allow us to express more things as **pure functions**

### Infinite Data Structures

Laziness lets us define data structures that extend infinitely.

Lists are a common example, but it also applies to trees or any user-defined data type:

``` hs
ones = 1 : ones
```

Many functions such as `take`, `drop`, `head`, `tail`, `filter` and `map` work fine on infinite lists

``` hs
naturals = 0 : map (+1) naturals
-- or
naturals = map sum (inits ones)
-- where inits generates all possible prefixes of a list
-- e.g. inits [1,2,3]
-- gives [[], [1], [1,2], [1,2,3]]

-- For Fibonacci numbers
fibs = 1:1:zipWith (+) fibs (tail fibs)
-- where zipWith takes a function and two lists
-- e.g. zipWith (+) [1,2,3] [10,20,30]
-- gives [11,22,33]
```

[^1]: J. Hughes, "Why Functional Programming Matters", Comp H., 1989
