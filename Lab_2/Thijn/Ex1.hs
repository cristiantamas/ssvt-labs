-- Lab 2 Exercises
-- Thijn Albers

module Exes where

import Lab2 ( Shape(..), (-->), probs )
import Data.List (permutations, deleteBy, delete, nub)
import Test.QuickCheck

-- Excercise 1
-- Time: 2 hours

categorize :: Float -> Integer
categorize x | x >= 0 && x < 0.25 = 1
             | x >= 0.25 && x < 0.5 = 2
             | x >= 0.5 && x < 0.75 = 3
             | x >= 0.75 && x <=1 = 4

categorizeList :: [Float] -> [Integer]
categorizeList = map categorize

countCategory :: [Integer] -> Integer -> Int
countCategory xs c = length (filter (== c) xs)

categorizeM :: IO [Float] -> IO [Integer]
categorizeM = fmap categorizeList

ex1 :: IO ()
ex1 = do
        cs <- categorizeM (probs 10000)
        putStrLn "== Testing RNG Distribution =="
        putStrLn "Amount in Q1"
        print (countCategory cs 1)
        putStrLn "Amount in Q2"
        print (countCategory cs 2)
        putStrLn "Amount in Q3"
        print (countCategory cs 3)
        putStrLn "Amount in Q4"
        print (countCategory cs 4)