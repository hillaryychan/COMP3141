-- Defintions
(++) :: [a] -> [a] -> [a]
(++) [] ys = ys                   -- 1
(++) (x:xs) ys = x : xs ++ ys     -- 2

reverse :: [a] -> [a]
reverse []     = []                -- A
reverse (x:xs) = reverse xs ++ [x] -- B

-- To Prove for all ls: reverse (reverse ls) == ls
-- First Prove for all ys: reverse (ys ++ [x]) = x:reverse ys

-- (1) Prove for all ys: reverse (ys ++ [x]) = x:reverse ys
-- Base Case: []
reverse ([] ++ [x]) = reverse([x])      -- 1
                    = reverse( x:[] )   -- def. of []
                    = reverse [] ++ [x] -- B
                    = [] ++ [x]         -- A
                    = [x]               -- 1
                    = x:[]              -- def. of []
                    = x:reverse []      -- A^

-- Inductive Hypothesis: ys
reverse (ys ++ [x]) == x:reverse ys

-- Recursive Case: y:ys
-- Want to show reverse(y:ys ++ [x]) == x:reverse (y:ys)
reverse((y:ys) ++ [x]) = reverse( y : (ys ++ [x]) )      -- 2
                       = reverse( ys ++ [x] ) ++ [y]     -- B
                       = (x:reverse ys) ++ [y]           -- I.H
                       = x : (reverse ys ++ [y])         -- 2
                       = x:reverse(y:ys)                 -- B

-- (2) Prove for all ls: reverse (reverse ls) == ls
-- Base Case: []
reverse (reverse []) = reverse []      -- A
                     = []              -- A

-- Inductive Hypothesis: ls
reverse (reverse ls) == ls

-- Recursive Case: x:xs
-- Want to show reverse (reverse (x:xs)) == x:xs
reverse (reverse (x:xs)) = reverse (reverse xs ++ [x])      -- B
                         = x: (reverse (reverse xs))        -- I.H (1)
                         = x:xs                             -- I.H (2)
