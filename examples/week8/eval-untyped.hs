data Expr t = BConst Bool
            | IConst Int
            | Times (Expr Int) (Expr Int)
            | Less (Expr Int) (Expr Int)
            | And (Expr Bool) (Expr Bool)
            | If (Expr Bool) (Expr t) (Expr t)
            deriving (Show, Eq)
data Value = BVal Bool | IVal Int
             deriving (Show, Eq)

eval :: Expr -> Value
eval (BConst b) = BVal b
eval (IConst i) = IVal i
eval (Times e1 e2) = case (eval e1, eval e2) of 
                       (IVal i1, IVal i2) -> IVal (i1 * i2)
eval (Less e1 e2)  = case (eval e1, eval e2) of
                       (IVal i1, IVal i2) -> BVal (i1 < i2) 
eval (And e1 e2)  = case (eval e1, eval e2) of
                       (BVal b1, BVal b2) -> BVal (b1 && b2) 
eval (If ec et ee) = 
  case eval ec of 
    BVal True  -> eval et
    BVal False -> eval ee
