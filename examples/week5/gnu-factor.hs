import Test.QuickCheck 
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic 
import System.Process

-- readProcess :: FilePath -> [String] -> String -> IO String

test_gnuFactor :: Positive Integer -> Property
test_gnuFactor (Positive n) = monadicIO $ do
    str <- run (readProcess "gfactor" [show n] "")
    let factors = map read (tail (words str))
    assert (product factors == n)

