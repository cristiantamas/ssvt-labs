{- File: Ex4.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 4
-}

-- TODO: How would you test this?

import Lab1

reversalPrimes :: [Integer]
reversalPrimes = filter (\x -> prime x && prime (reversal x)) [1..10000]

main :: IO ()
main = do
  putStrLn "\n== Reversable primes under 10000 =="
  print reversalPrimes

  putStrLn ""