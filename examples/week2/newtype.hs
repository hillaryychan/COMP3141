newtype X = X Bool
  deriving (Show)
instance Semigroup X where
  X a <> X b = X a
