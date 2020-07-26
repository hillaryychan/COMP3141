{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, GADTs #-}
{-# LANGUAGE UndecidableInstances, StandaloneDeriving #-}

module Vector where

-- Natural numbers, values will be promoted to types
data Nat
  = Z     -- Zero
  | S Nat -- Successor (+1)

data Vec (a :: *) :: Nat -> * where
  -- Nil has zero length
  Nil :: Vec a Z
  -- Cons has length of the tail + 1
  Cons :: a -> Vec a n -> Vec a (S n)

deriving instance Show a => Show (Vec a n)

-- head :: [a] -> a
hd :: Vec a (S n) -> a
hd (Cons x xs) = x

-- tail :: [a] -> [a]
tl :: Vec a (S n) -> Vec a n
tl (Cons x xs) = xs

-- map :: (a -> b) -> [a] -> [b]
vMap :: (a -> b) -> Vec a n -> Vec b n
vMap f Nil = Nil
vMap f (Cons x xs) = Cons (f x) (vMap f xs)

-- (++) :: [a] -> [a] -> [a]
vAppend :: Vec a n -> Vec a m -> Vec a (Plus n m)
vAppend Nil         xs = xs
vAppend (Cons y ys) xs = Cons y (vAppend ys xs)

-- Type-level addition
type family Plus (x :: Nat) (y :: Nat) :: Nat where
  Plus Z     n = n
  Plus (S m) n = S (Plus m n)


-- concat :: [[a]] -> [a]
vConcat :: Vec (Vec a n) m -> Vec a (Times m n)
vConcat Nil = Nil
vConcat (Cons xs xss) = xs `vAppend` vConcat xss

-- Type-level multiplication
type family Times (x :: Nat) (y :: Nat) :: Nat where
  Times Z     m = Z
  Times (S n) m = Plus m (Times n m)

vFilter :: (a -> Bool) -> Vec a n -> [a]
vFilter p Nil = []
vFilter p (Cons x xs)
  | p x = x : vFilter p xs
  | otherwise = vFilter p xs
