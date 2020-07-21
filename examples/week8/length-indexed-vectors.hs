{-# LANGUAGE GADTs, KindSignatures #-}
{-# LANGUAGE DataKinds, StandaloneDeriving, TypeFamilies #-}

data Nat = Z | S Nat

plus :: Nat -> Nat -> Nat 
plus Z n = n
plus (S m) n = S (plus m n)

type family Plus (m :: Nat) (n :: Nat) :: Nat where
  Plus Z n = n
  Plus (S m) n = S (Plus m n)

data Vec (a :: *) :: Nat -> * where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

deriving instance Show a => Show (Vec a n)

appendV :: Vec a m -> Vec a n -> Vec a (Plus m n)
appendV Nil ys         = ys
appendV (Cons x xs) ys = Cons x (appendV xs ys)

-- 0: Z
-- 1: S Z
-- 2: S (S Z)

hd :: Vec a (S n) -> a
hd (Cons x xs) = x


mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec f Nil = Nil
mapVec f (Cons x xs) = Cons (f x) (mapVec f xs)
