(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) Nothing f = Nothing
(>>=) (Just a) f = f a

-- Example
db :: [(ZID, Name)]
db = [(3253158, "Liam"),
      (8888888, "Rich"),
      (4444444, "Mort")]

studentNames :: [ZID] -> Maybe [Name]
studentNames [] = pure []
studentNames (z:zs) = do 
     n  <- lookup z db
     ns <- studentNames zs
     pure (n:ns)

-- briefer but less clear with applicative notation:
-- studentNames (z:zs) = (:) <$> lookup z db <*> studentNames zs
