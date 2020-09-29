-- Lab 4.5
-- Time: 1hr
-- Author: Thijn Albers

import Data.List
import SetOrd

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Haskell Road (and workshop):
-- The transative closure of R is R+ which is the union of R^1 ... R^n
-- until adding R^n+1 does not add new elements to the union
-- r1 = R^1, rn = R^n, rc = r1 union r2 .. union rn-1 .. union rn
trClos' :: Ord a => Rel a -> Rel a -> Rel a -> Rel a
trClos' r1 rn rc    | union rc (r1 @@ rn) == rc = rc
                    | otherwise = trClos' r1 (r1 @@ rn) (rc `union` (r1 @@ rn))

trClos :: Ord a => Rel a -> Rel a
trClos r = trClos' r r r