-- Lab 4
-- Exercise 1
-- Time: 1 hr

module Ex1 where

import Data.List
import SetOrd
import Test.QuickCheck
import System.Random


{- An easy way to generate random sets is to generate random lists of integers and then call `list2set`
 - (SetOrd) on that list. Using monads for this is very convenient.
-}
genRandomSet :: IO (Set Int)
genRandomSet = do list2set <$> genIntList

{- A generator of random lists I found in these lecture slides:
 - https://staff.fnwi.uva.nl/d.j.n.vaneijck2/courses/16/fsa/lectures/FSA2.html
-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0, n))

randomFlip :: Int -> IO Int
randomFlip x = do
  b <- getRandomInt 1
  if b == 0 then return x else return (- x)

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
  x <- getRandomInt k
  y <- randomFlip x
  xs <- getIntL k (n -1)
  return (y : xs)

{- With quickcheck we can do the same thing using the built-in generator
 - for a random int list. I choose an interval for the integers, since the size of the set
 - is important, but the size of the elements in it not so much.
-}
genRandomSetQC :: Gen (Set Int)
genRandomSetQC = list2set <$> listOf (choose (-1000, 1000))