{- File: Ex1.hs
 - Author: Leon Kielstra
 - Lab 1 Exercise 1
-}
import Debug.Trace
import Test.QuickCheck

-- Workshop ex 2
sumSquares :: Integer -> Integer
sumSquares n = sum [k ^ 2 | k <- [1 .. n]]

sumSquares' :: Integer -> Integer
sumSquares' n = div (n * (n + 1) * (2 * n + 1)) 6

testSumSquares :: Integer -> Bool
testSumSquares n = let a = abs n in sumSquares a == sumSquares' a

-- Workshop ex 3
sumCubes :: Integer -> Integer
sumCubes n = sum [k ^ 3 | k <- [1 .. n]]

sumCubes' :: Integer -> Integer
sumCubes' n = (div (n * (n + 1)) 2) ^ 2

testSumCubes :: Integer -> Bool
testSumCubes n = let a = abs n in sumCubes a == sumCubes' a

-- QuickCheck test cases
main :: IO ()
main = do
  putStrLn "== Proof of induction sum of squares =="
  quickCheckResult testSumSquares

  putStrLn "\n== Proof of induction sum of cubes =="
  quickCheckResult testSumCubes

  putStrLn ""
