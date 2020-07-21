eval :: Expr -> Maybe Value
eval (BConst b) = pure (BVal b)
eval (IConst i) = pure (IVal i)
eval (Times e1 e2) = do
     v1 <- eval e1
     v2 <- eval e2
     case (v1,v2) of
       (IVal v1', IVal v2') -> pure (IVal (v1' * v2'))
       _ -> Nothing
eval (Less e1 e2) = do
     v1 <- eval e1
     v2 <- eval e2
     case (v1,v2) of
       (IVal v1', IVal v2') -> pure (BVal (v1' < v2'))
       _ -> Nothing
eval (And e1 e2) = do
     v1 <- eval e1
     v2 <- eval e2
     case (v1,v2) of
       (BVal v1', BVal v2') -> pure (BVal (v1' && v2'))
       _ -> Nothing
eval (If ec et ee) = do
     v1 <- eval ec
     case v1 of
       (BVal True) -> eval et
       (BVal False) -> eval ee
