{- File: Ex2.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 2
-}

import Data.List (subsequences)
import Test.QuickCheck (quickCheckResult, withMaxSuccess)

lenPowerSet :: [Int] -> Int
lenPowerSet xs = length (subsequences xs)

lenPowerSet' :: [Int] -> Int
lenPowerSet' xs = 2 ^ length xs

-- QuickCheck test cases
{-
  This test is becoming really slow really easily,
  since the length increases exponentionally.
-}
testPowerSet :: [Int] -> Bool
testPowerSet xs = lenPowerSet xs == lenPowerSet' xs

main :: IO ()
main = do
  putStrLn "\n== Proof of induction cardinality of powerset =="
  quickCheckResult (withMaxSuccess 25 testPowerSet)

  putStrLn ""