module Measure where

newtype Measure p u = M Float

data One
data Square p
data Kilometers
data Miles

distance :: Float -> Measure One Kilometers
distance = M

imDistance :: Float -> Measure One Miles
imDistance = M

square :: Measure p u -> Measure (Square p) u
square (M i) = M $ i * i

data Stage = One | Two | Final

descentStage :: Measure One Kilometers -> Stage
descentStage (M 125) = One
descentStage (M 50)  = Two
descentStage (M 1)   = Final

imToMet :: Measure One Miles -> Measure One Kilometers
imToMet (M i) = M $ i * 1.60934

s = descentStage $ imToMet $ imDistance 70
