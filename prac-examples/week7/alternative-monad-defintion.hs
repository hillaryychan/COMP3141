--
-- class Applicative m => Monad m where
--   join :: m (m a) -> m a

import Control.Monad

-- Implement in terms of >>=
join' :: Monad m => m (m a) -> m a
join' = (>>= id)

-- Implement in terms of join
(>>=!) :: Monad m => m a -> (a -> m b) -> m b
pre >>=! post = join $ fmap post pre
