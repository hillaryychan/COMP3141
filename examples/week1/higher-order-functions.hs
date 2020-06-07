twice :: (a -> a) -> (a -> a)
twice f a = f (f a) -- Equation 1

double :: Int -> Int
double = \x -> x * 2


{-
   twice twice double 3
== (twice twice double) 3  - Equation 1
== (twice (twice double)) 3 
== (twice quadruple) 3 - defn. of quadruple
== quadruple (quadruple 3) - Equation 1
== 48
-}

quadruple :: Int -> Int
quadruple = twice double -- defn of quadruple

