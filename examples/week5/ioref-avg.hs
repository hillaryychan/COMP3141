import Data.IORef
import Test.QuickCheck.Monadic
import Test.QuickCheck

averageListIO :: [Int] -> IO Int
averageListIO ls = do
    sum <- newIORef 0
    count <- newIORef 0
    let loop :: [Int] -> IO ()
        loop [] = pure ()
        loop (x:xs) = do
            s <- readIORef sum
            writeIORef sum (s + x)
            c <- readIORef count
            writeIORef count (c + 1)
            loop xs
    loop ls
    s <- readIORef sum
    c <- readIORef count
    pure (s `div` c)

prop_average :: [Int] -> Property
prop_average ls = monadicIO $ do
    pre (length ls > 0)
    avg <- run (averageListIO ls)
    assert (avg == (sum ls `div` length ls))

