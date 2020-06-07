maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)

maybeMap f mx = case mx of
                  Nothing -> Nothing
                  Just x  -> Just (f x)


-- type level:
-- (,) :: * -> (* -> *)
-- (,) x :: * -> *

instance Functor ((,) x) where
--  fmap :: (a -> b) -> f a -> f b
--  fmap :: (a -> b) -> (,) x a -> (,) x b
--  fmap :: (a -> b) -> (x,a) -> (x, b)
  fmap f (x,a) = (x, f a)


-- type level:
-- (->) :: * -> (* -> *)
-- (->) x :: * -> *

instance Functor ((->) x) where
--  fmap :: (a -> b) -> f a -> f b
--  fmap :: (a -> b) -> (->) x a -> (->) x b
--  fmap :: (a -> b) -> (x -> a) -> (x -> b)
  fmap = (.)

