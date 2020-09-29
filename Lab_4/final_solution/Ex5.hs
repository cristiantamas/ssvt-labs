-- Lab 4
-- Exercise 5

module Ex5 where

import Data.List (nub, sort)

--- Definitions from exercise ---
type Rel a = [(a, a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

--------

{- The transitive closure of a relation R can be obtained by taking the union of R^1 .. R^n while R^n != R^(n+1)
 - This can be denoted by R+, https://en.wikipedia.org/wiki/Transitive_closure. For this while loop we can use a
 - fixed point operation as described in Lecture 4 slide 8.
-}
trClos :: Ord a => Rel a -> Rel a
trClos = fp (nub . sort . composition)

-- Composition of R^n and R^n+1
composition :: Eq a => Rel a -> Rel a
composition relation = relation ++ (relation @@ relation)

fp :: Eq a => (a -> a) -> a -> a
fp f = until (\x -> x == f x) f