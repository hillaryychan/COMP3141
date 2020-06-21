module Ransom where

import Test.QuickCheck
import Data.Char
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

type Magazine = String
type Message  = String

-- 1. Specify the ransom note check
rnoteSpec :: Magazine -> Message -> Bool
rnoteSpec magazine message = all enoughChars message
  where
    enoughChars :: Char -> Bool
    enoughChars c = countChars c magazine >= countChars c message
    countChars :: Char -> String -> Int
    countChars c = length . filter (==c)

newtype Counts = Counts (M.Map Char Int) deriving (Eq, Show)

instance Ord Counts where
  (Counts sub) <= (Counts super) = hasChars && hasSufficient
    where
      subChars = M.keysSet sub
      superChars = M.keysSet super
      hasChars = subChars `S.isSubsetOf` superChars
      hasSufficient = and $ M.intersectionWith (<=) sub super

emptyCounts = Counts M.empty

increment :: Char -> Counts -> Counts
increment char (Counts counts) = Counts $ M.alter inc char counts
  where
    inc :: Maybe Int -> Maybe Int
    inc Nothing = Just 1
    inc (Just n) = Just $ n + 1

countChars :: String -> Counts
countChars = foldr increment emptyCounts

-- 2. How can you implement a more efficient checking mechanism?
rnote :: Magazine -> Message -> Bool
rnote magazine message = messageCounts <= magazineCounts
  where
    magazineCounts = countChars magazine
    messageCounts = countChars message

-- 3. Show that it is equivalent using properties
prop_rnote message magazine =
  rnoteSpec message magazine == rnote message magazine
