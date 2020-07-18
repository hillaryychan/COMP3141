data Tree a = Leaf 
            | Branch a (Tree a) (Tree a) 
            deriving (Show, Eq)

instance Arbitrary (Tree Int) where
  arbitrary = do
      mn <- (arbitrary :: Gen Int)
      Positive delta <- arbitrary
      let mx = mn + delta
      searchTree mn mx
    where
      searchTree :: Int -> Int -> Gen (Tree Int)
      searchTree mn mx 
         | mn >= mx = pure Leaf
         | otherwise = do
            v <- choose (mn,mx)
            l <- searchTree mn v
            r <- searchTree (v+1) mx
            pure (Branch v l r)
