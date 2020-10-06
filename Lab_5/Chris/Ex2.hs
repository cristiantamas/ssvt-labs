-- Lab 5
-- Exercise 2
-- Time: 1 hr

import Data.List
import Test.QuickCheck
import System.Random
import Lecture5


-- The function checks all the numbers between x, where 2 <= x < n
-- We get a list with results for mrComposite for all the numbers in the interval
-- If we can find some results that are true, then n is a composite 
is_composite:: Integer -> Bool
is_composite n = (takeWhile (\x -> mrComposite x n == True) [2..n-1]) /= []


-- Composite number generator
-- For testing puroses and to not crash anything, this was tested the interval 1 .. 100
-- The results are below
composites' :: [Integer]
composites' = filter (\x -> is_composite x == True) [1..]


--main :: IO()
--main = do
--    print $ composites'

-- List of composite numbers between 1 and 100
-- [4,6,8,9,10,12,14,15,16,18,20,21,22,24,25,26,27,
-- 28,30,32,33,34,35,36,38,39,40,42,44,45,46,48,49,
-- 50,51,52,54,55,56,57,58,60,62,63,64,65,66,68,69,
-- 70,72,74,75,76,77,78,80,81,82,84,85,86,87,88,90,
-- 91,92,93,94,95,96,98,99,100]