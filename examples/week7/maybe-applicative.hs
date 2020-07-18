type Name = String
type ZID = Int
data Program = COMP | SENG | BINF | CENG deriving (Show, Eq)
type StudentRecord = (Name, ZID, Program)

lookupID :: Name -> Maybe ZID
lookupID "Liam" = Just 3253158
lookupID "Unlucky" = Just 4444444
lookupID "Prosperous" = Just 8888888
lookupID _ = Nothing

lookupProgram :: Name -> Maybe Program
lookupProgram "Liam" = Just COMP
lookupProgram "Unlucky" = Just SENG
lookupProgram "Prosperous" = Just CENG
lookupProgram _ = Nothing

makeRecord :: ZID -> Program -> Name -> StudentRecord
makeRecord zid pr name = (name,zid,pr)



liam :: Maybe StudentRecord
liam = let mzid = lookupID "Liam"
           mprg = lookupProgram "Liam"
        in pure makeRecord <*> mzid <*> mprg <*> pure "Liam"

--     pure :: a -> Maybe a
--     fmap :: (a -> b) -> Maybe a -> Maybe b
