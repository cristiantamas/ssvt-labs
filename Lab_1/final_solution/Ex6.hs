{- File: Ex6.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 6
-}

import Lab1
import Data.List

conjecture :: Integer -> Integer
conjecture n = product (take (fromIntegral n) primes) + 1

testConjecture :: Integer -> Bool
testConjecture n = prime (conjecture n)

-- Smallest Counter Example is 7
counterConjecture :: [Integer]
counterConjecture = filter doesNotHold primes
                        where doesNotHold x = not (testConjecture x)

main :: IO ()
main = do
  putStrLn "\n== Counter Conjecture =="
  print (take 3 counterConjecture)

  putStrLn ""