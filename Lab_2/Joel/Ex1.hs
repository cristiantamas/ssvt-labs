module Ex1 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


-- Red Curry claim correctness

--Time: 1 hour

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

calcQuartile :: [Float] -> [Float] -> [Int]
calcQuartile randomList (x1:x2:rest) = length(filter (\x -> x>=x1 && x<x2) randomList):calcQuartile randomList (x2:rest)
calcQuartile x y = []

--Run with
--probs 10000 >>= \y -> return (calcQuartile y [0, 0.25..1])

--Result = [2477,2494,2444,2585]. All quarters get an equal amount thus proving that the random generator is working correctly.
