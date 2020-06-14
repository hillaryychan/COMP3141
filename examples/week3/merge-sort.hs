split :: [a] -> ([a],[a])
split [] = ([],[])
split [a] = ([a],[])
split (x:y:xs) = let (l,r) = split xs
                  in (x:l,y:r)


prop_splitPerm xs = let (l,r) = split (xs :: [Int])
                     in permutation xs (l ++ r)

permutation :: (Ord a) => [a] -> [a] -> Bool
permutation xs ys = sort xs == sort ys

permutation' :: (Eq a) => [a] -> [a] -> (a -> Bool)
permutation' xs ys = \x -> count x xs == count x ys
  where
    count x l = length (filter (== x) l)


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

prop_mergePerm xs ys = permutation (xs ++ (ys :: [Int] )) (merge xs ys)

prop_mergeSorted (Ordered xs) (Ordered ys) = sorted (merge (xs :: [Int]) ys)

sorted :: Ord a => [a] -> Bool
sorted [] = True 
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (l,r) = split xs
                in merge (mergeSort l) (mergeSort r)


prop_mergeSortSorts xs = sorted (mergeSort (xs :: [Int])) 

prop_mergeSortPerm xs = permutation xs (mergeSort (xs :: [Int]))

prop_mergeSortExtra xs = mergeSort (xs :: [Int]) == sort xs

prop_mergeSortUnit = mergeSort [3,2,1] == [1,2,3]


main = do
  quickCheck prop_mergeSortUnit
  quickCheck prop_mergeSortSorts
  quickCheck prop_mergeSortPerm
