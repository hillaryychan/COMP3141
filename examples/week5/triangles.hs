printTriangle :: Int -> IO ()
printTriangle 0 = pure ()
printTriangle n = do
  putStrLn (replicate n '*')
  printTriangle (n - 1)

main = printTriangle 9
