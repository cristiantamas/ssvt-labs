-- Lab4
-- Exercise 2
-- Time 2 hr

{-# LANGUAGE FlexibleInstances #-}

module Ex2 where


import Data.List (intersect, union, (\\))
import Ex1
import SetOrd
import Test.QuickCheck

{- For these functions we can use the intersect, union and \\ (difference)
 - functions for lists, since we can easily extract the list from the set using
 - pattern matching.
-}
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set list1) (Set list2) = list2set (list1 `intersect` list2)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set list1) (Set list2) = list2set (list1 `union` list2)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set list1) (Set list2) = list2set (list1 \\ list2)

--- Helper functions not in SetOrd ---
set2list :: Ord a => Set a -> [a]
set2list (Set list) = list

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

{- Test properties.
 - Found some useful properties here: http://web.mnstate.edu/peil/MDEV102/U1/S3/Property6.htm
-}

--- Properties on itself ---
-- Intersection and Union on a set itself results in the same set.
propIntersectionSelf, propUnionSelf :: Ord a => Set a -> Bool
propIntersectionSelf s = (setIntersection s s) == s
propUnionSelf s = (setUnion s s) == s

-- Difference on a set itself equals an empty set.
propDifferenceSelf :: Ord a => Set a -> Bool
propDifferenceSelf s = (setDifference s s) == emptySet

--- Identity properties ---
-- For the intersection between sets each element should be in both sets
propIntersection :: Ord a => Set a -> Set a -> Bool
propIntersection s1 s2 =
  subSet intersection s1 && subSet intersection s2
  where
    intersection = (setIntersection s1 s2)

-- The intersection between a set and the empty set should result in an empty set
propIntersectionEmpty :: Ord a => Set a -> Bool
propIntersectionEmpty s = (setIntersection s emptySet) == emptySet

-- For the union between sets each element should be in at least one of the sets
propUnion :: Ord a => Set a -> Set a -> Bool
propUnion s1 s2 =
  all (\n -> inSet n s1 || inSet n s2) (set2list (setUnion s1 s2))

-- The union between a set and the empty set should result in the given set
propUnionEmpty :: Ord a => Set a -> Bool
propUnionEmpty s = (setUnion s emptySet) == s

-- For the difference between sets each element should be in only one set
propDifference :: Ord a => Set a -> Set a -> Bool
propDifference s1 s2 =
  all (\n -> inSet n s1 `xor` inSet n s2) (set2list (setDifference s1 s2))

--- Commutative properties ---
-- Intersection and Union are commutative
propIntersectionCommutative, propUnionCommutative :: Ord a => Set a -> Set a -> Bool
propIntersectionCommutative s1 s2 = (setIntersection s1 s2) == (setIntersection s2 s1)
propUnionCommutative s1 s2 = (setUnion s1 s2) == (setUnion s2 s1)

--- Associative properties ---
-- Intersection and Union are associative
propIntersectionAssociative, propUnionAssociative :: Ord a => Set a -> Set a -> Set a -> Bool
propIntersectionAssociative s1 s2 s3 =
  setIntersection (setIntersection s1 s2) s3 == setIntersection s1 (setIntersection s2 s3)
propUnionAssociative s1 s2 s3 =
  setUnion (setUnion s1 s2) s3 == setUnion s1 (setUnion s2 s3)

--- Distributive properties ---
propIntersectionDistributive, propUnionDistributive :: Ord a => Set a -> Set a -> Set a -> Bool
propIntersectionDistributive s1 s2 s3 =
  setIntersection s1 (setUnion s2 s3) == setUnion (setIntersection s1 s2) (setIntersection s1 s3)
propUnionDistributive s1 s2 s3 =
  setUnion s1 (setIntersection s2 s3) == setIntersection (setUnion s1 s2) (setUnion s1 s3)

--- Running the tests ---
instance Arbitrary (Set Int) where
  arbitrary = genRandomSetQC

runTests :: Show a => IO a -> (a -> Bool) -> Int -> Int -> IO ()
runTests gen test times current
  | times == current = print ("+++ OK, passed " ++ show times ++ " tests.")
  | otherwise = do
    s <- gen
    if test s
      then runTests gen test times (current + 1)
      else print ("Failed for: " ++ show s)

runTests2 :: Show a => IO a -> (a -> a -> Bool) -> Int -> Int -> IO ()
runTests2 gen test times current
  | times == current = print ("+++ OK, passed " ++ show times ++ " tests.")
  | otherwise = do
    s1 <- gen
    s2 <- gen
    if test s1 s2
      then runTests2 gen test times (current + 1)
      else print ("Failed for: " ++ show s1 ++ "\n" ++ show s2)

runTests3 :: Show a => IO a -> (a -> a -> a -> Bool) -> Int -> Int -> IO ()
runTests3 gen test times current
  | times == current = print ("+++ OK, passed " ++ show times ++ " tests.")
  | otherwise = do
    s1 <- gen
    s2 <- gen
    s3 <- gen
    if test s1 s2 s3
      then runTests3 gen test times (current + 1)
      else print ("Failed for: " ++ show s1 ++ "\n" ++ show s2 ++ "\n" ++ show s3)

ex2 :: IO ()
ex2 = do
  --- Own generator ---
  -- Properties on itself
  runTests genRandomSet propIntersectionSelf 100 0 -- "+++ OK, passed 100tests."
  runTests genRandomSet propUnionSelf 100 0 -- "+++ OK, passed 100tests."
  runTests genRandomSet propDifferenceSelf 100 0 -- "+++ OK, passed 100tests."
  -- Identity properties
  runTests2 genRandomSet propIntersection 100 0 -- "+++ OK, passed 100tests."
  runTests genRandomSet propIntersectionEmpty 100 0 -- "+++ OK, passed 100tests."
  runTests2 genRandomSet propUnion 100 0 -- "+++ OK, passed 100tests."
  runTests genRandomSet propUnionEmpty 100 0 -- "+++ OK, passed 100tests."
  runTests2 genRandomSet propDifference 100 0 -- "+++ OK, passed 100tests."
  -- Commutative properties
  runTests2 genRandomSet propIntersectionCommutative 100 0 -- "+++ OK, passed 100tests."
  runTests2 genRandomSet propUnionCommutative 100 0 -- "+++ OK, passed 100tests."
  -- Associative properties
  runTests3 genRandomSet propIntersectionAssociative 100 0 -- "+++ OK, passed 100tests."
  runTests3 genRandomSet propUnionAssociative 100 0 -- "+++ OK, passed 100tests."
  -- Distributive properties
  runTests3 genRandomSet propIntersectionDistributive 100 0 -- "+++ OK, passed 100tests."
  runTests3 genRandomSet propUnionDistributive 100 0 -- "+++ OK, passed 100tests."

  --- QuickCheck tests ---
  -- Properties on itself
  quickCheck $ forAll genRandomSetQC propIntersectionSelf -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propUnionSelf -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propDifferenceSelf -- +++ OK, passed 100 tests.
  -- Identity properties
  quickCheck $ forAll genRandomSetQC propIntersection -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propIntersectionEmpty -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propUnion -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propUnionEmpty -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propDifference -- +++ OK, passed 100 tests.
  -- Commutative properties
  quickCheck $ forAll genRandomSetQC propIntersectionCommutative -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propUnionCommutative -- +++ OK, passed 100 tests.
  -- Associative properties
  quickCheck $ forAll genRandomSetQC propIntersectionAssociative -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propUnionAssociative -- +++ OK, passed 100 tests.
  -- Distributive properties
  quickCheck $ forAll genRandomSetQC propIntersectionDistributive -- +++ OK, passed 100 tests.
  quickCheck $ forAll genRandomSetQC propUnionDistributive -- +++ OK, passed 100 tests.
