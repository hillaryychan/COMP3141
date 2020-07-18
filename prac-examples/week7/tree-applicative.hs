data Tree a
   = Leaf
   | Node a (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

instance Applicative Tree where
    -- pure :: a -> Tree a
    pure v = Node v Leaf Leaf

    -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    Leaf <*> _    = Leaf
    _    <*> Leaf = Leaf
    (Node f fl fr) <*> (Node x xl xr) =
      Node (f x) (fl <*> xl) (fr <*> xr)
