import Test.QuickCheck
import Data.Char
import Data.List
--                 Testable          
--                 Arbitrary    Testable
--                               Arbitrary    Testable
prop_reverseApp :: [Int]     -> ([Int]     -> Bool)
prop_reverseApp xs ys =
   reverse (xs ++ ys) == reverse ys ++ reverse xs



divisible :: Int -> Int -> Bool
divisible x y = x `mod` y == 0

prop_refl :: Positive Int -> Bool 
prop_refl (Positive x) = divisible x x


prop_unwordsWords s = unwords (words s) == s
prop_wordsUnwords l = all (\w -> all (not . isSpace) w && w /= []) l 
                    ==> words (unwords l) == l

