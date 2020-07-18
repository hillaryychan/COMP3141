pureZ :: a -> [a]
pureZ a = a:pureZ a

applyListsZ :: [a -> b] -> [a] -> [b]
applyListsZ (f:fs) (x:xs) = f x : applyListsZ fs xs
applyListsZ [] _ = []
applyListsZ _ [] = []

pureC :: a -> [a]
pureC a = [a] 

applyListsC :: [a -> b] -> [a] -> [b]
applyListsC (f:fs) args = map f args ++ applyListsC fs args
applyListsC [] args = []
