module Ex4 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ i:j | i <- xs, j <- perms $ delete i xs ]

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs

deran :: Int -> [[Int]]
deran n = filter (\ p -> isDerangement p [0..n-1]) (perms [0..n-1])

testDerOne, testDerTwo, testDerThree, testDerFour :: Bool
testDerOne = isDerangement [1,2,3] [2,3,1] == True
testDerTwo = isDerangement [1,2,3] [1,3,2] == False
testDerThree = isDerangement [(-1),5,2,7] [5,(-1),7,2] == True
testDerFour = isDerangement [(-1),5,2,7] [(-1),5,7,2] == False

--I don't exactly know how the stronger / weaker property works and how to make an automated test
