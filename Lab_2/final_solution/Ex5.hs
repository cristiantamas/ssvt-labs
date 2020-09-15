{- File: Ex5.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 2 Exercise 5 (Recognizing and generating derangements)
-}

import Data.List (delete, deleteBy, nub, permutations)
import Lab2 (Shape (..), probs, (-->))
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys =
  length xs == length ys
    && forall xs (`elem` ys)

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement xs ys =
  isPermutation xs ys
    && forall xsAndys (uncurry (/=))
  where
    xsAndys = zip xs ys

deran :: [Integer] -> [[Integer]]
deran xs = filter (`isDerangement` xs) (permutations xs)

ordered :: [Integer] -> Bool
ordered [] = True
ordered [x] = True
ordered [x1, x2] = x1 < x2
ordered (x1 : x2 : xs) = x1 < x2 && ordered (x2 : xs)

-- Testable properties:
-- If a is a derangement of b, a is ordered and b is not (list should be non-empty)
prop_isDeranOrder :: [Integer] -> [Integer] -> Bool
prop_isDeranOrder xs ys = isDerangement xs ys --> ordered xs && not (ordered ys)

-- If a is a derangement of b, a and b have equal length
prop_isDeranLen :: [Integer] -> [Integer] -> Bool
prop_isDeranLen xs ys = isDerangement xs ys --> length xs == length ys

-- If a is a derangement of b, neither lists contain duplicate elements
prop_isDeranDup :: [Integer] -> [Integer] -> Bool
prop_isDeranDup xs ys =
  isDerangement xs ys --> (length (nub xs) == length xs)
    && (length (nub ys) == length ys)

ex5 :: IO ()
ex5 = do
  quickCheck $ forAll (listOf1 arbitrary) prop_isDeranOrder
  quickCheck prop_isDeranLen
  quickCheck prop_isDeranDup
