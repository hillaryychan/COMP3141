data ZList a = Nil
             | Cons a (ZList a)
             deriving Show

instance Functor ZList where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative ZList where
  pure x = Cons x (pure x)

  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)
  _ <*> _ = Nil
