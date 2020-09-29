-- Lab 4.1
-- Time: 1.5hr
-- Author: Thijn Albers

{-# LANGUAGE FlexibleInstances #-}
import Data.List
import SetOrd
import Test.QuickCheck
import System.Random
import Control.Monad

-- TODO: complete own random generator

randomElement :: IO Int
randomElement = randomRIO (1, 10^2::Int)

instance Arbitrary (Set Int) where
    arbitrary = randomSetQ

randomSet :: IO (Set Int)
randomSet = list2set <$> replicateM 10 randomElement

-- quickcheck Generator
posInt :: Gen Int
posInt = abs `fmap` (arbitrary:: Gen Int) `suchThat` (> 0)

-- use listOf instead of listOf1 because we want to include the empty set in our testcases
randomSetQ :: Gen (Set Int)
randomSetQ = list2set <$> listOf posInt