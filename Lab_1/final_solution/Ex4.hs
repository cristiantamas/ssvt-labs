{- File: Ex4.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 4
-}

{-
- To test:
  - The reversal function can be tested with QuickCheck. A function that e.g. reverses the number twice and tests equality.
  - There are some edge cases such as negative numbersf of numbers that start with zero that we do not currently account for
  - but we are only concerned with prime numbers.

  - For complete certainty, besides the reversal function the filter and prime functions should also be tested.
-}
import Lab1

reversalPrimes :: [Integer]
reversalPrimes = filter (\x -> prime x && prime (reversal x)) [1..10000]

main :: IO ()
main = do
  putStrLn "\n== Reversable primes under 10000 =="
  print reversalPrimes

  putStrLn ""