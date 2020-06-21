module ListProps where
import Test.QuickCheck

-- Lists

-- Proof of associativity of list semigroup

prop_semigroup :: [Int] -> [Int] -> [Int] -> Bool
prop_semigroup xs ys zs = ((xs ++ ys) ++ zs) == (xs ++ (ys ++ zs))

-- Proof of identity of list monoid

-- Left identity
prop_leftIdentity :: [Int] -> Bool
prop_leftIdentity xs = xs == [] ++ xs

-- Right itdentity

prop_rightIdentity :: [Int] -> Bool
prop_rightIdentity xs =  xs == xs ++ []

-- List reverse

prop_involution :: [Int] -> Bool
prop_involution xs = xs == reverse (reverse xs)
