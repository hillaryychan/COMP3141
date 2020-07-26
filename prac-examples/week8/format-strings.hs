module Format where

{-
 - Format
 - ======
 -
 - Add type of expected arguments
 -
 - * End of format string
 - * String literal
 - * Decimal argument
 - * String argument
 -}

{-
data Format
  = End -- End :: Format
  | L String Format -- L :: String -> Format -> Format
  | Dec Format -- Dec :: Format -> Format
  | Str Format -- Str :: Format -> Format
  deriving (Show, Eq)
-}

data Format :: * -> * where
  End :: Format ()
  L :: String -> Format a -> Format a
  Dec :: Format a -> Format (Int, a)
  Flt :: Format a -> Format (Float, a)
  Str :: Format a -> Format (String, a)

deriving instance Show (Format a)

printf :: Format a -> a -> IO ()
printf End () = putChar '\n'
printf (L literal fs) as = do
  putStr literal
  printf fs as
printf (Dec fs) (i, as) = do
  putStr $ show (i :: Int)
  printf fs as
printf (Flt fs) (i, as) = do
  putStr $ show (i :: Float)
  printf fs as
printf (Str fs) (s, as) = do
  putStr s
  printf fs as

-- "Hello %s you are %d years old"
example = L "Hello " $ Str $ L " you are " $ Dec $ L " years old" End
