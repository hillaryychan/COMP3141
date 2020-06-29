import Control.Monad.State 

data Tree a = Branch a (Tree a) (Tree a) | Leaf 
     deriving (Show, Eq)

-- label in infix order, starting at 1.
label :: Tree () -> Tree Int 
label t = snd (go t 1)
 where
   go :: Tree () -> Int -> (Int, Tree Int)
   go Leaf c = (c, Leaf)
   go (Branch () l r) c = let 
      (c', l') = go l c
      v = c'
      (c'',r') = go r (c'+1)
    in (c'', Branch v l' r')


label' :: Tree () -> Tree Int
label' t = evalState (go t) 1 
  where
    go :: Tree () -> State Int (Tree Int)
    go Leaf = pure Leaf
    go (Branch () l r) = do
       l' <- go l
       v  <- get
       put (v + 1)
       r' <- go r
       pure (Branch v l' r')


