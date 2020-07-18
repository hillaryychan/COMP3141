studentNames :: [ZID] -> Either ZID [Name]
studentNames [] = pure []
studentNames (z:zs) = do 
     n  <- case lookup z db of
             Just v -> Right v
             Nothing -> Left z
     ns <- studentNames zs
     pure (n:ns)
