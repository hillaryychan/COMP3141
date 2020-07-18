--
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   tuple :: f a -> f b -> f (a, b)

import Control.Applicative

-- fmap f x = f <$> x = pure f <*> x

-- Implement using fmap, pure, and <*>
tuple :: Applicative f => f a -> f b -> f (a, b)
tuple l r = ((,) <$> l) <*> r

-- Implement <*> using fmap, pure, and tuple
(<*!>) :: Applicative f => f (a -> b) -> f a -> f b
f <*!> x = uncurry ($) <$> tuple f x

-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f (l, r) = f l r
