module Dictionary
  ( Word
  , Definition
  , Dict
  , emptyDict
  , insertWord
  , lookup
  ) where

import Prelude hiding (Word, lookup)
import Test.QuickCheck
import Test.QuickCheck.Modifiers
-- lookup :: [(a,b)] -> a -> Maybe b
type Word = String
type Definition = String

newtype Dict = D [DictEntry]
             deriving (Show, Eq)

emptyDict :: Dict
emptyDict = D []

insertWord :: Word -> Definition -> Dict -> Dict
insertWord w def (D defs) = D (insertEntry (Entry w def) defs)
  where
    insertEntry wd (x:xs) = case compare (word wd) (word x)
                              of GT -> x : (insertEntry wd xs)
                                 EQ -> wd : xs
                                 LT -> wd : x : xs
    insertEntry wd [] = [wd]

lookup :: Word -> Dict -> Maybe Definition
lookup w (D es) = search w es
  where
    search w [] = Nothing
    search w (e:es) = case compare w (word e) of
       LT -> Nothing
       EQ -> Just (defn e)
       GT -> search w es

sorted :: (Ord a) => [a] -> Bool
sorted []  = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

wellformed :: Dict -> Bool
wellformed (D es) = sorted es

prop_insert_wf dict w d = wellformed dict ==>
                          wellformed (insertWord w d dict)

data DictEntry
  = Entry { word :: Word
          , defn :: Definition
          } deriving (Eq, Show)

instance Ord DictEntry where
  Entry w1 d1 <= Entry w2 d2 = w1 <= w2


instance Arbitrary DictEntry where
  arbitrary = Entry <$> arbitrary <*> arbitrary

instance Arbitrary Dict where
  arbitrary = do
    Ordered ds <- arbitrary
    pure (D ds)

prop_arbitrary_wf dict = wellformed dict
