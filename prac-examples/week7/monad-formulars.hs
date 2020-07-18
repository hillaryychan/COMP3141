import Control.Monad (ap)

data Variable = A | B | C deriving (Show,Eq)

data Formula v = Var v
               | And (Formula v) (Formula v)
               | Or  (Formula v) (Formula v)
               | Not (Formula v)
               | Constant Bool
               deriving (Eq,Show)

infixr /\
(/\) :: Formula v -> Formula v -> Formula v
a /\ b = And a b

infixr \/
(\/) :: Formula v -> Formula v -> Formula v
a \/ b = Or a b
-- a \/ b = Not $ (Not a) /\ (Not b)

example :: Formula Variable
example = (Var A /\ Var A) \/ (Not (Var B) /\ Var C)
-- Or (And (Var A) (Var A)) (And (Not (Var B)) (Var C))

instance Functor Formula where -- fmap is renaming variables
  -- fmap :: (a->b) -> Formula a -> Formula b
  fmap f (Var v) = Var $ f v
  fmap f (And l r) = And (fmap f l) (fmap f r)
  fmap f (Or  l r) = Or  (fmap f l) (fmap f r)
  fmap f (Not x) = Not $ fmap f x
  fmap f (Constant b) = Constant b

 -- try fmap (const B) example

 -- Applicatives don't make much sense here, so we can use
 -- the `ap` function from Control.Monad to implement <*>
 -- in terms of >>=, which is easier in this case to write than <*>:
instance Applicative Formula where
  -- pure :: a -> Formula a
  pure = Var

  -- (<*>) :: Formula (a -> b) -> Formula a -> Formula b
  (<*>) = ap

  -- ap :: Monad m => m (a -> b) -> m a -> m b
  -- ap f x = do
  --   f' <- f
  --   x' <- x
  --   pure $ f' x'

instance Monad Formula where  -- >>= is substitution
  -- (>>=) :: Formula a -> (a -> Formula b) -> Formula b
  (Var v)      >>= f = f v
  (And l r)    >>= f = And (l >>= f) (r >>= f)
  (Or  l r)    >>= f = Or  (l >>= f) (r >>= f)
  (Not x)      >>= f = Not $ x >>= f
  (Constant b) >>= f = Constant b

subst A = Constant True
subst B = Constant False
subst C = Constant True

-- try `example >>= subst`

-- Evaluate a formula with no variables
evalFormula :: Formula a -> Maybe Bool
evalFormula (Var a) = Nothing
evalFormula (And l r) = (&&) <$> evalFormula l <*> evalFormula r
evalFormula (Or l r) = do
  { l' <- evalFormula l
  ; r' <- evalFormula r
  ; pure $ l' || r'
  }
evalFormula (Not x) = not <$> evalFormula x
evalFormula (Constant b) = pure b
