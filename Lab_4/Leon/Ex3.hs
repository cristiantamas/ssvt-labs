{- File: Ex3.hs
 - Author: Leon Kielstra
 - Lab 4 Exercise 3
-}

module Ex3 where

import SetOrd (Set (..), list2set, unionSet)

-- Also defined in Ex2
set2list :: Ord a => Set a -> [a]
set2list (Set list) = list

type Rel a = [(a, a)]

{- Definition of symmetric closure according to https://en.wikipedia.org/wiki/Symmetric_closure
 - Union of R and the transpose of R (all elements switched in the relation)
-}
symClos :: Ord a => Rel a -> Rel a
symClos relation = set2list (unionSet (list2set relation) (list2set (transpose relation)))

transpose :: Ord a => Rel a -> Rel a
transpose relation = [(x, y) | (y, x) <- relation]
