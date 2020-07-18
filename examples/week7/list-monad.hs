(>>=) :: [a] -> (a -> [b]) -> [b]
(>>=) as f = concatMap f as

-- Example
roll :: [Int]
roll = [1,2,3,4,5,6]


diceGame = do
  d1 <- roll
  d2 <- roll
  if (abs (d1 - d2) < 2) then do
     d2' <- roll
     pure (abs (d1 - d2'))
  else
     pure (abs (d1 - d2))
