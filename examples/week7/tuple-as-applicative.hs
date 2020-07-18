instance Functor ((,) x) where
  fmap :: (a -> b) -> (x,a) -> (x,b)
  fmap f (x,a) = (x,f a)

instance Monoid x => Applicative ((,) x) where
  pure :: a -> (x,a)
  pure a = (mempty ,a)

  (<*>) :: (x,a -> b) -> (x, a) -> (x, b)
  (<*>) (x, f) (x',a) = (x <> x', f a)

-- required Monoid here to combine values, and to provide a default value for pure
f :: A -> (Log, B)
g :: X -> (Log, Y)
a :: A
x :: X
combine :: B -> Y -> Z


test :: (Log, Z)
test = combine <$> f a <*> g x
-- instead of 
--  let (l1, b) = f a
--      (l2, y) = g x
--   in (l1 <> l2, combine b y)
