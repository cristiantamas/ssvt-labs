{- File: Ex3.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 3
-}

import Data.List (permutations)
import Test.QuickCheck (quickCheckResult, withMaxSuccess)

lengthPermutations :: [Integer] -> Integer
lengthPermutations xs = factorial (toInteger (length xs))

factorial :: Integer -> Integer
factorial n = product [1 .. n]

lengthPermutations' :: [Integer] -> Integer
lengthPermutations' xs = toInteger (length (permutations xs))

-- QuickCheck test cases
{- This is hard to test, because the numbers are increasing exponentionally
 - because of the factorial.
-}
testLengthPermutations :: [Integer] -> Bool
testLengthPermutations xs = lengthPermutations xs == lengthPermutations' xs

-- On testing with QuickCheck:
-- By testing with QuickCheck you are not proofing that your solution is correct.
-- You are merely showing that it works for a lot of cases,
-- which can strongly indicate that it works for most, if not all input.
-- Using QuickCheck (or testing by generating samples in general) is never as air tight as mathematical proof.
main :: IO ()
main = do
  putStrLn "\n== Proof of induction lenght of permutations =="
  quickCheckResult (withMaxSuccess 12 testLengthPermutations)

  putStrLn ""