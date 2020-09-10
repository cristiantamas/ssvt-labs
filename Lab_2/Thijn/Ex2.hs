-- Lab 2 Exercises
-- Thijn Albers

module Exes where

import Lab2 ( Shape(..), (-->), probs )
import Data.List (permutations, deleteBy, delete, nub)
import Test.QuickCheck

-- Excercise 2
-- Time: implementation 1 hr, evaluation testing 1 hr

twoSidesEqual :: Integer -> Integer -> Integer -> Bool
twoSidesEqual a b c = length (nub [a, b, c]) == 2

threeSidesEqual :: Integer -> Integer -> Integer -> Bool
threeSidesEqual a b c = length (nub [a, b, c]) == 1

isProperTriangle :: Integer -> Integer -> Integer -> Bool
isProperTriangle a b c = mx < (a + b + c - mx) where mx = maximum [a, b, c]

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | not (isProperTriangle a b c) = NoTriangle
                | threeSidesEqual a b c = Equilateral
                | a ^ 2 + b ^ 2 == c ^ 2 = Rectangular
                | twoSidesEqual a b c = Isosceles
                | otherwise = Other

prop_isNoTriangle :: Integer -> Integer -> Integer -> Bool
prop_isNoTriangle a b c = triangle a b c == NoTriangle

prop_isEquilateral :: Integer -> Integer -> Integer -> Bool
prop_isEquilateral a b c = triangle a b c == Equilateral

prop_isRectengular :: Integer -> Integer -> Integer -> Bool
prop_isRectengular a b c = triangle a b c == Rectangular

prop_isIsosceles :: Integer -> Integer -> Integer -> Bool
prop_isIsosceles a b c = triangle a b c == Isosceles

prop_isOther :: Integer -> Integer -> Integer -> Bool
prop_isOther a b c = triangle a b c == Other

ex2 :: IO ()
ex2 = do
        -- Could write such tests for the other properties aswell
        -- However, this would just be the using the same code / ideas
        -- to generate test cases.. Not sure how to properly create automated tests
        quickCheck (\n -> n > 0 ==> prop_isEquilateral n n n)

        -- You can also test with basic examples
        print (prop_isRectengular 3 4 5)
        print (prop_isIsosceles 10 10 5)
        print (prop_isNoTriangle 12 4 50)