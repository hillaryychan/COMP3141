{-# LANGUAGE DataKinds, KindSignatures #-}

data Stream = UG | PG
data StudentID (x :: Stream) = SID Int

-- data Either a b = Left a | Right b

postgrad :: [Int]
postgrad = [3253158]

makeStudentID :: Int -> Either (StudentID UG) (StudentID PG)
makeStudentID i | i `elem` postgrad = Right (SID i) 
                | otherwise         = Left  (SID i)

enrollInCOMP3141 :: StudentID UG -> IO ()
enrollInCOMP3141 (SID x) 
  = putStrLn (show x ++ " enrolled in COMP3141!")
