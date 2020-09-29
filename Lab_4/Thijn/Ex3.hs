-- Lab 4.3
-- Time: 10 min
-- Author: Thijn Albers

import Data.List
import SetOrd
import Test.QuickCheck

-- From Ex2.hs
set2list :: Ord a => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = x : set2list (Set xs)
-- / From ex2.hs

type Rel a = [(a,a)]

-- Haskell Road:
-- The union of R with R^-1 (inverse) is the symmetric closure of r
symClos :: Ord a => Rel a -> Rel a
symClos r = set2list $ unionSet r_set r_inv
            where r_set = list2set r
                  r_inv = list2set [(y, x) | (x, y) <- r]
