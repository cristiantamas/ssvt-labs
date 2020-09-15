-- Lab 2 Exercises
-- Thijn Albers

import Lab2 ( Shape(..), (-->), probs )
import Data.List (permutations, deleteBy, delete, nub)
import Test.QuickCheck

-- Ex 3.1
-- Time: implementation: 10 minutes
--       trying test function for properties: 1.5 hours

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


-- Ex 3.2
-- Time: 2 hours
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

ex3 :: IO ()
ex3 = do
        quickCheckWith stdArgs { maxSize = 10 } testIsPermutation
        quickCheckWith stdArgs { maxSize = 10 } testIsPermutation'
        quickCheckWith stdArgs { maxSize = 10 } testIsNotPermutation
        quickCheck prop_PermComp
        quickCheck prop_PermImplication
        quickCheck prop_PermLength