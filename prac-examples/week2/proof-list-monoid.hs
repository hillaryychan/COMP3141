-- Defintions
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys           -- 1
(++) (x:xs) ys = x : xs ++ ys -- 2

-- Prove for all xs, ys, zs: ((xs ++ ys) ++ zs) ==  (xs ++ (ys ++ zs))
-- Additionally prove
-- (1) for all xs: [] ++ xs == xs
-- (2) for all xs: xs ++ [] == xs

-- (1) Left identity - for all xs: [] ++ xs == xs
[] ++ xs = xs              -- 1

-- (2) Right identity - for all xs: xs ++ [] == xs
-- Base case: []
[] ++ [] = []          -- 1

-- Inductive Hypothesis
xs ++ [] = xs

-- Recursive case:
-- Want to show (x:xs) ++ == x:xs
(x:xs) ++ [] = x : (xs + [])     -- 2
             = x : xs            -- I.H

-- (3) Prove for all xs, ys, zs: ((xs ++ ys) ++ zs) ==  (xs ++ (ys ++ zs))
-- Base case: []
(([] ++ ys) ++ zs) = ys ++ zs          -- 1
                   = [] ++ (ys ++ zs)  -- 1^

-- Inductive Hypothesis
((xs ++ ys) ++ zs) ==  (xs ++ (ys ++ zs))

-- Recursive case:
-- Want to show ((x:xs ++ ys) ++ zs) ==  (x:xs ++ (ys ++ zs))
((x:xs ++ ys) ++ zs) = (x : (xs ++ ys)) ++ zs    -- 2
                     = x : ((xs ++ ys) ++ zs)    -- 2
                     = x : (xs ++ (ys ++ zs))    -- I.H
                     = x:xs ++ (ys ++ zs)        -- 2^
