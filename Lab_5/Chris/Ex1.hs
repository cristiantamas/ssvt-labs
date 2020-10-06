-- Lab 5
-- Exercise 1
-- Time: 2 hr

import Data.List
import Test.QuickCheck
import System.Random
import Lecture5

-- Inspired by https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/
-- Found during researching for modular exponentiation
exM' :: Integer -> Integer -> Integer -> Integer
exM' x 0 m = 1
exM' x e m 
    | e `mod` 2 == 0 = ((exM' x (e `div` 2) m) * (exM' x (e `div` 2) m)) `mod` m
    | otherwise = (x * (exM' x (e `div` 2) m) * (exM' x (e `div` 2) m)) `mod` m


test_exM' :: Integer -> Integer -> Integer -> Bool
test_exM' x e m = (x^e) `mod` m == exM' x e m

--main :: IO()
--main = do

--    quickCheck $ test_exM' 3 200 50
--    quickCheck $ test_exM' 2 90 13
--    quickCheck $ test_exM' 3 2 4
