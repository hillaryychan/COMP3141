import Control.Monad.State

{-
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)
-}



newtype State' s a = State (s -> (s, a))

get' :: State' s s
get' = (State $ \s -> (s, s))

put' :: s -> State' s ()
put' s = State $ \_ -> (s,())

pure' :: a -> State' s a
pure' a = State $ \s -> (s, a)

evalState' :: State' s a -> s -> a
evalState (State f) s = snd (f s)

(>>=!) :: State' s a -> (a -> State' s b) -> State' s b
(State c) >>=! f = State $ \s -> let (s', a) = c s
                                    (State c') = f a
                                 in c' s'

(>>!) :: State' s a -> State' s b -> State' s b
(>>!) a b = a >>=! \_ -> b

