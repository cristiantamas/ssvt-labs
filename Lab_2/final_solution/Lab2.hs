module Lab2 where

import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n -1)
  return (p : ps)

data Shape
  = NoTriangle
  | Equilateral
  | Isosceles
  | Rectangular
  | Other
  deriving (Eq, Show)
