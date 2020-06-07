-- in maths: f(g(x)) == (f o g)(x)

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = (f x)  :  (myMap f xs)

-- 1 : 2 : 3 : []
-- 1 + 2 + 3 + 0
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum xs


-- ["hello","world","!"] -> "helloworld!"
-- "hello":"world":"!":[]
-- "hello"++"world"++"!"++[]

concat' :: [[a]] -> [a]
concat' []        = []
concat' (xs:xss)  = xs ++ concat xss

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []     = z
foldr' f z (x:xs) = x `f` (foldr' f z xs)

sum'' = foldr' (+) 0
concat'' = foldr' (++) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
-- filter' p (x:xs) = if p x then x : filter' p xs 
--                          else filter' p xs
filter' p (x:xs) 
   | p x       = x : filter' p xs
   | otherwise = filter' p xs 
