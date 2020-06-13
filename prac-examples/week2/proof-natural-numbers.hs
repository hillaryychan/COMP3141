-- Defintions
data Nat = Zero
         | Succ Nat

add :: Nat -> Nat -> Nat
add Zero     n = n              -- 1
add (Succ a) b = add a (Succ b) -- 2

one = Succ Zero                 -- 3
two = Succ (Succ Zero)          -- 4

-- Prove one `add` one = two
add one one = add (Succ Zero) (Succ Zero) -- 3 and 3
            = add Zero (Succ (Succ Zero)) -- 2
            = Succ (Succ Zero)            -- 1
            = two                         -- 2^

