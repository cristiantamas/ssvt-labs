{- File: Ex1.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 1
-}

import Test.QuickCheck (quickCheckResult, forAll)
import Lab1

-- Workshop ex 2
sumSquares :: Integer -> Integer
sumSquares n = sum [k ^ 2 | k <- [1 .. n]]

sumSquares' :: Integer -> Integer
sumSquares' n = div (n * (n + 1) * (2 * n + 1)) 6

testSumSquares :: Integer -> Bool
testSumSquares n = sumSquares n == sumSquares' n

-- Workshop ex 3
sumCubes :: Integer -> Integer
sumCubes n = sum [k ^ 3 | k <- [1 .. n]]

sumCubes' :: Integer -> Integer
sumCubes' n = div (n ^ 2 + n) 2 ^ 2

testSumCubes :: Integer -> Bool
testSumCubes n = sumCubes n == sumCubes' n

-- QuickCheck test cases
main :: IO ()
main = do
  putStrLn "== Proof of induction sum of squares =="
  quickCheckResult $ forAll genPositiveIntegers testSumSquares

  putStrLn "\n== Proof of induction sum of cubes =="
  quickCheckResult $ forAll genPositiveIntegers testSumCubes

  putStrLn ""