import Data.List
import System.IO

getListItem :: [Int] -> String

getListItem [] = "Your list is empty"
getListItem (x:[]) = "Your list starts with " ++ show x
getListItem (x:y:[]) = "Your list cotains " ++ show x ++ " and " ++ show y
getListItem (x:xs) = "The 1st item is " ++ show x ++ " and rest is " ++ show xs

getFirstItem :: String -> String

getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first item in " ++ all ++ " is " ++ [x]

areStringEq :: [Char] -> [Char] -> Bool

areStringEq [] [] = True
areStringEq (x:xs) (y:ys) = x == y && areStringEq xs ys
areStringEq _ _ = False
