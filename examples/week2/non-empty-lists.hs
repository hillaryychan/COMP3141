data NonEmpty a = One a | Cons a (NonEmpty a)

safeHead :: NonEmpty a -> a
safeHead (One a) = a
safeHead (Cons a _) = a


append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append (One a) ls = Cons a ls
append (Cons a as) ls = Cons a (append as ls)
