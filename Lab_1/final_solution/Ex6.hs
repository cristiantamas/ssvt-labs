{- File: Ex6.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 6
-}

import Lab1
import Data.List

conjecture :: Integer -> Integer
conjecture n = product (take (fromIntegral n) primes) + 1

testConjecture :: Integer -> Bool
testConjecture n = not (prime (conjecture n))

-- Smallest is 7
counterConjecture :: [Integer]
counterConjecture = filter testConjecture primes

main :: IO ()
main = do
  putStrLn "\n== Counter Conjecture =="
  print (take 1 counterConjecture)

  putStrLn ""