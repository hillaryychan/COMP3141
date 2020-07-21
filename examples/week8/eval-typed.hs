{-# LANGUAGE GADTs, KindSignatures #-}
data Expr :: * -> * where
  BConst :: Bool -> Expr Bool
  IConst :: Int -> Expr Int
  Times  :: Expr Int -> Expr Int -> Expr Int
  Less   :: Expr Int -> Expr Int -> Expr Bool
  And    :: Expr Bool -> Expr Bool -> Expr Bool
  If     :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr t -> t
eval (IConst i)    = i
eval (BConst b)    = b
eval (Times e1 e2) = eval e1 * eval e2
eval (Less e1 e2)  = eval e1 < eval e2
eval (And e1 e2)   = eval e1 && eval e2
eval (If ec et ee) = if eval ec then eval et else eval ee
