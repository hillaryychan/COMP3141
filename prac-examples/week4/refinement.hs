import Test.QuickCheck

-- Abstract Specification / Model
data Abstract = A { text :: String, cursor :: Int }
   deriving (Show, Eq)

einitA :: String -> Abstract
einitA s = A s (length s)

stringOfA :: Abstract -> String
stringOfA (A s c) = s

moveLeftA :: Abstract -> Abstract
moveLeftA (A t c)
  | c > 0     = A t (c - 1)
  | otherwise = A t c

moveRightA :: Abstract -> Abstract
moveRightA (A t c)
  | c < length t = A t (c + 1)
  | otherwise    = A t c

insertCharA :: Char -> Abstract -> Abstract
insertCharA x (A t c) = A (left ++ [x] ++ right) (c + 1)
  where
    (left, right) = splitAt c t

deleteCharA :: Abstract -> Abstract
deleteCharA (A t c)
  | c >= length t = A t c
  | otherwise     = A deleted c
  where
    deleted = left ++ tail right
    (left, right) = splitAt c t

-- What are our data *invariants* ?

prop_wellformedAbstract :: Abstract -> Bool
prop_wellformedAbstract (A t c)
  = 0 <= c && c <= length t

-- Constructors

prop_arbitrary_wf :: Abstract -> Bool
prop_arbitrary_wf = prop_wellformedAbstract

prop_einitA_wf :: String -> Bool
prop_einitA_wf s = prop_wellformedAbstract $ einitA s

-- Mutators

prop_moveLeftA_wf :: Abstract -> Property
prop_moveLeftA_wf a
  = prop_wellformedAbstract a
  ==> prop_wellformedAbstract (moveLeftA a)

prop_moveRightA_wf :: Abstract -> Property
prop_moveRightA_wf a
  = prop_wellformedAbstract a
  ==> prop_wellformedAbstract (moveRightA a)

prop_insertCharA_wf :: Char -> Abstract -> Property
prop_insertCharA_wf c a
  = prop_wellformedAbstract a
  ==> prop_wellformedAbstract (insertCharA c a)

prop_deleteCharA_wf :: Abstract -> Property
prop_deleteCharA_wf a
  = prop_wellformedAbstract a
  ==> prop_wellformedAbstract (deleteCharA a)

-- Concrete Implementation
data Editor = E String String deriving (Show, Eq)

einit :: String -> Editor
einit s = E (reverse s) ""

stringOf :: Editor -> String
stringOf (E left right)
  = reverse left ++ right

moveLeft :: Editor -> Editor
moveLeft (E (l:ls) rs) = E ls (l:rs)
moveLeft (E []     rs) = E [] rs

moveRight :: Editor -> Editor
moveRight (E ls (r:rs)) = E (r:ls) rs
moveRight (E ls []    ) = E ls     []

insertChar :: Char -> Editor -> Editor
insertChar x (E ls rs) = E (x:ls) rs

deleteChar :: Editor -> Editor
deleteChar (E ls (r:rs)) = E ls rs
deleteChar (E ls []    ) = E ls []

-- Data invariants

-- Every possible value of Editor is well-formed

-- Abstraction function
toAbstract :: Editor -> Abstract
toAbstract (E ls rs) = A
  { text = reverse ls ++ rs
  , cursor = length ls
  }

-- Refinement
--
-- To show that fA refines f:
--  f . toAbstract == toAbstract . fA
--
--       a --- fA ---> a'
--       ^             ^
--       |             |
-- toAbstract       toAbstract
--       |             |
--       |             |
--       e ---- f ---> e'

prop_einit_rf :: String -> Bool
prop_einit_rf s
  = toAbstract (einit s) == einitA s

prop_moveLeftA_rf :: Editor -> Bool
prop_moveLeftA_rf e
  = toAbstract (moveLeft e) == moveLeftA (toAbstract e)

prop_moveRightA_rf :: Editor -> Bool
prop_moveRightA_rf e
  = toAbstract (moveRight e) == moveRightA (toAbstract e)

prop_insertCharA_rf :: Char -> Editor -> Bool
prop_insertCharA_rf c e
  = toAbstract (insertChar c e) == insertCharA c (toAbstract e)

prop_deleteCharA_rf :: Editor -> Bool
prop_deleteCharA_rf e
  = toAbstract (deleteChar e) == deleteCharA (toAbstract e)

-- don't worry about this too much for now
instance Arbitrary Abstract where
  arbitrary = do
    t <- arbitrary
    c <- choose (0, length t)
    pure (A t c)

instance Arbitrary Editor where
  arbitrary = E <$> arbitrary <*> arbitrary

