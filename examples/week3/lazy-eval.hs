{-# LANGUAGE BangPatterns #-}
sumTo' !a 0 = a
sumTo' !a n = sumTo' (a+n) (n-1)

-- sumTo' 0 5
-- sumTo' (0+5) (5-1)
-- sumTo' (0+5) 4
-- sumTo' (0+5+4) (4-1)
-- sumTo' (0+5+4) 3
-- sumTo' (0+5+4+3) (3-1)
-- sumTo' (0+5+4+3) 2
-- ..

-- sumTo' 0 100000
-- sumTo' (0 + 100000 + 99999 + 99998 + 99997 + ....) 0
-- 

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = sumTo (n-1) + n
