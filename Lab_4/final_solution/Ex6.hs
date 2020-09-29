-- Lab 4
-- Exercise 6

module Ex6 where

import Data.List (nub, sort)
import Ex3 (symClos, transpose)
import Ex5 (trClos, (@@))
import Test.QuickCheck ( choose, listOf, forAll, quickCheck, Gen )

type Rel a = [(a, a)]

--- The two most obvious properties to test are symmetry and transitivity. ---
-- For symmetry the transpose of the result should have the same elements
propSymmetry :: (Eq a, Ord a) => Rel a -> Bool
propSymmetry relation = sort (transpose relation) == sort relation

-- Every symmetric closure is symmetric
propSymClosSymmetric :: (Eq a, Ord a) => Rel a -> Bool
propSymClosSymmetric relation = propSymmetry (symClos relation)

-- Every relation is contained in it's own symmetric closure
propSymClosContained :: (Eq a, Ord a) => Rel a -> Bool
propSymClosContained relation = all (\x -> x `elem` symClos relation) relation

-- For property of transitive closure holds that the composition of relations results in the same relation
propTransitivity :: (Eq a, Ord a) => Rel a -> Bool
propTransitivity relation = all (`elem` relation) (relation @@ relation)

-- Every transitive closure is transitive
propTrClosTransitive :: (Eq a, Ord a) => Rel a -> Bool
propTrClosTransitive relation = propTransitivity (trClos relation)

-- Every relation is contained in it's own transitive closure
propTrClosContained :: (Eq a, Ord a) => Rel a -> Bool
propTrClosContained relation = all (\x -> x `elem` trClos relation) relation

--- To be able to generate Quickcheck tests we need a generator ---
genRandomRelation :: Gen (Rel Int)
genRandomRelation = listOf genRandomPair

{- We pick a domain between 1 and 10, because otherwise the computations take too long. The size of the elements
 - are not important for these tests.
-}
genRandomPair :: Gen (Int, Int)
genRandomPair = do
  x <- choose (1, 10)
  y <- choose (1, 10)
  return (x, y)

--- QuickCheck testing
ex6 :: IO ()
ex6 = do
  quickCheck $ forAll genRandomRelation propSymClosSymmetric -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomRelation propSymClosContained -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomRelation propTrClosTransitive -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomRelation propTrClosContained -- +++ OK, passed 100 tests.