instance Applicative ((->) x) where
  pure :: a -> x -> a
  pure a x = a

  (<*>) :: (x -> (a -> b)) -> (x -> a) -> (x -> b)
  (<*>) xab xa x = xab x (xa x)

-- f (g x) (h x) (i x)
--
-- Can be written as: 
-- (pure f <*> g <*> h <*> i) x
