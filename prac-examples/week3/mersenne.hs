module Mersenne where

import Test.QuickCheck

nats :: [Int]
nats = 0 : map (+1) nats

-- Seive of Eratosthenes
primes :: [Int]
primes = 2 : rest
  where
    rest = filter isPrime $ drop 3 nats

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p /= 0) smallerPrimes
  where
    smallerPrimes :: [Int]
    smallerPrimes = takeWhile (\p -> p * p <= n) primes

prop_mersenne :: Positive Int -> Property
prop_mersenne (Positive n) = isPrime n ==> isPrime $ (2 ^ n) - 1

