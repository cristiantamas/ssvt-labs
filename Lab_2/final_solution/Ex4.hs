{- File: Ex4.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 2 Exercise 4 (Recognizing Permutations)
-}

import Data.List (delete, deleteBy, nub, permutations)
import Lab2 (Shape (..), probs, (-->))
import Test.QuickCheck

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys =
  length xs == length ys
    && forall xs (`elem` ys)

testIsPermutation :: [Integer] -> Bool
testIsPermutation a = forall (permutations a) (`isPermutation` a)

testIsPermutation' :: [String] -> Bool
testIsPermutation' a = forall (permutations a) (`isPermutation` a)

-- Adding one more element makes it impossible to be a permutation of itself
testIsNotPermutation :: [Int] -> Bool
testIsNotPermutation a = not (forall (permutations a) (`isPermutation` (a ++ [1])))

-- Testable properties:
-- If a is a permutation of b, than b is a permutation of a
prop_PermComp :: [Integer] -> [Integer] -> Bool
prop_PermComp a b = isPermutation a b == isPermutation b a

-- If a is a permutation of b, and b is a permutation of c, than a is a permutation of c
prop_PermImplication :: [Integer] -> [Integer] -> [Integer] -> Bool
prop_PermImplication a b c = isPermutation a b && isPermutation b c --> isPermutation a c

-- Because we dont need to account for duplicates, a and be can be permutations if and only if they are the same length
prop_PermLength :: [Integer] -> [Integer] -> Bool
prop_PermLength a b = isPermutation a b --> length a == length b

ex4 :: IO ()
ex4 = do
  quickCheckWith stdArgs {maxSize = 10} testIsPermutation
  quickCheckWith stdArgs {maxSize = 10} testIsPermutation'
  quickCheckWith stdArgs {maxSize = 10} testIsNotPermutation
  quickCheck prop_PermComp
  quickCheck prop_PermImplication
  quickCheck prop_PermLength
