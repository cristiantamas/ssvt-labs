-- Lab 2 Exercises
-- Thijn Albers

module Exes where

import Lab2 ( Shape(..), (-->), probs )
import Data.List (permutations, deleteBy, delete, nub)
import Test.QuickCheck

-- Ex 3
-- Time: way too long
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

prop_1 :: Integer -> Bool
prop_1 x =  even x && x > 3

prop_2 :: Integer -> Bool
prop_2 x = even x || x > 3

prop_3 :: Integer -> Bool
prop_3 x = (even x && x > 3) || even x

-- :)
descendingOrder :: [[Char]]
descendingOrder = ["prop_1", "even", "prop_3", "prop_2"]


-- Ex 4
-- Time: nvt
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys =   length xs == length ys
                        && forall xs (`elem` ys)

testIsPermutation :: [Integer] -> Bool
testIsPermutation a = forall (permutations a) (`isPermutation` a)

testIsPermutation' :: [String] -> Bool
testIsPermutation' a = forall (permutations a) (`isPermutation` a)

-- Adding one more element makes it impossible to be a permutation of itself
testIsNotPermutation :: [Int] -> Bool
testIsNotPermutation a = not (forall (permutations a) (`isPermutation` (a ++ [1])))

ex3 :: IO ()
ex3 = do
        quickCheckWith stdArgs { maxSize = 10 } testIsPermutation
        quickCheckWith stdArgs { maxSize = 10 } testIsPermutation'
        quickCheckWith stdArgs { maxSize = 10 } testIsNotPermutation