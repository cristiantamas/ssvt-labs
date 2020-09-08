{- File: Ex5.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 5
-}

import Lab1

primes101 :: [Integer] -> [Integer]
primes101 xs
  | prime (sum (take 101 xs)) = take 101 xs -- Base case
  | otherwise = primes101 (tail xs) -- Recursion

-- Solution
solution :: Integer
solution = sum (primes101 primes)

{- Unless you have a different formula of which you are confident that it is correct, the closest
 - way to test this is by testing the functions that are being used: prime, sum, take, tail, and primes.
-}
main :: IO ()
main = do
  putStrLn "\n== Smallest prime that is a sum of 101 primes =="
  print solution

  putStrLn ""