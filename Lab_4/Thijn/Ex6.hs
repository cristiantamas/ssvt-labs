-- Lab 4.6
-- Time: 1hr so far
-- Author: Thijn Albers

-- TODO: Add random checks that are not quickCheck. I think thats what they want?
--       Add testcases that should give false
--       Make order necessary for closure functions
--       Add more testcases / properties

import Data.List
import SetOrd
import Control.Monad
import Test.QuickCheck

-- From Ex3.hs
set2list :: Ord a => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = x : set2list (Set xs)

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = set2list $ unionSet r_set r_inv
            where r_set = list2set r
                  r_inv = list2set [(y, x) | (x, y) <- r]
-- / From Ex3.hs

-- From Ex4.hs
relInt :: Gen Int
relInt = abs `fmap` (arbitrary:: Gen Int) `suchThat` (> 0)

relPair :: Gen (Int, Int)
relPair = liftM2 (,) relInt relInt

genRelInt :: Gen (Rel Int)
genRelInt = listOf1 relPair
-- / From Ex4.hs

-- From Ex5.hs
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos' :: Ord a => Rel a -> Rel a -> Rel a -> Rel a
trClos' r1 rn rc    | union rc (r1 @@ rn) == rc = rc
                    | otherwise = trClos' r1 (r1 @@ rn) (rc `union` (r1 @@ rn))

trClos :: Ord a => Rel a -> Rel a
trClos r = trClos' r r r
-- / From Ex5.hs

-- Source: Haskell road Chapter 5.3 page 179
transR :: Ord a => Rel a -> Bool
transR [] = True
transR s = and [ trans pair (Set s) | pair <- s ] where
    trans (x,y) (Set r) = and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ]

-- Source: Haskell road Chapter 5.3 page 179
symR :: Ord a => Rel a -> Bool
symR [] = True
symR ((x,y):pairs)    | x == y = symR pairs
                            | otherwise =
                            inSet (y,x) pairs'
                            && symR (set2list (deleteSet (y,x) pairs'))
                            where pairs' = list2set pairs

-- Check if two lists are the same disregarding order
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements l1 l2 = all (`elem` l1) l2 && all (`elem` l2) l1

-- The transitive closure of R contains R
prop_trClosSub :: Rel Int -> Bool
prop_trClosSub r = all (`elem` trc) r where trc = trClos r

-- The symmetric closure of R contains R
prop_symClosSub :: Rel Int -> Bool
prop_symClosSub r = all (`elem` scr) r where scr = symClos r


-- The transR/symR function use Sets which means they are ordered, the functions I wrote
-- returns closures that are not order sensitive.
-- To at least randomly test the function I solved this as can be seen below.
-- This is obviously awful, but I valued the ability to random test higher than
-- having a maxSize parameter on the lists (for now)

-- The transitive closure of R is transitive
prop_trClosTrans :: Rel Int -> Bool
prop_trClosTrans r = any transR (permutations (trClos r))

-- The symmetric closure of r is symmetric
prop_symClosSym :: Rel Int -> Bool
prop_symClosSym r = any symR (permutations (symClos r))


ex6 :: IO ()
ex6 = do
    -- The examples from the lab
    putStrLn "=== Test Lab Examples ==="
    print $ symClos [(1,2),(2,3),(3,4)] `sameElements`  [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
    print $ trClos [(1,2),(2,3),(3,4)] `sameElements` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
    -- The example of the workshop
    let r = [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
    print $ trClos r == union r (r @@ r)

    putStrLn "=== Testing Symmetric Closure ==="
    quickCheck prop_symClosSub
    quickCheckWith stdArgs {maxSize=5} $ forAll genRelInt prop_symClosSym

    putStrLn "=== Testing Transitive Closure ==="
    quickCheck prop_trClosSub
    quickCheckWith stdArgs {maxSize=5} $ forAll genRelInt prop_trClosTrans

    -- === Test Lab Examples ===
    -- True
    -- True
    -- True
    -- === Testing Symmetric Closure ===
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- === Testing Transitive Closure ===
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.