-# LANGUAGE KindSignatures #-}
module Lec4 where

{-
-- Definition of Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

  -- Functor laws
  -- fmap id == id
  -- (fmap g) . (fmap f) == fmap (g . f)

-- Externally, the function `fmap` can be seen as having the following
-- type
fmap :: (Functor f) => (a -> b) -> f a -> f b
-}

-- Implementation of a list with an instance of Functor
data List a
  = Cons a (List a)
  | Nil
  deriving (Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap f Nil         = Nil

{-
-- Implementation of functor for functions of any input
instance Functor ((->) x) where
  fmap :: (a -> b) -> (x -> a) -> (x -> b)
  fmap f1 f2 = f1 . f2

-- Using fmap on the list
fmap (+1) [0, 1, 2] = [1, 2, 3]
fmap (+1) (Cons 0 (Cons 1 (Cons 2 Nil)))
  = (Cons 1 (Cons 2 (Cons 3 Nil)))

-- Using fmap on a function
(fmap (+1) (+2)) 3 == 6
-}
