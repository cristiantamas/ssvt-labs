-- Lab 4
-- Exercise 3
-- Time: 10 min

module Ex3 where

import Ex2 (set2list)
import SetOrd (list2set, unionSet)

type Rel a = [(a, a)]

{- Definition of symmetric closure according to https://en.wikipedia.org/wiki/Symmetric_closure and Haskell Road
 - Union of R and the transpose of R (all elements switched in the relation)
-}
symClos :: Ord a => Rel a -> Rel a
symClos relation = set2list (unionSet (list2set relation) (list2set (transpose relation)))

transpose :: Ord a => Rel a -> Rel a
transpose relation = [(x, y) | (y, x) <- relation]
