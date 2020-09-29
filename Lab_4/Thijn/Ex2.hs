-- Lab 4.2
-- Time: 2hr
-- Author: Thijn Albers

{-# LANGUAGE FlexibleInstances #-}
import Data.List
import SetOrd
import Test.QuickCheck
import System.Random
import Control.Monad

-- From Ex1.hs
instance Arbitrary (Set Int) where
    arbitrary = randomSetQ

posInt :: Gen Int
posInt = abs `fmap` (arbitrary:: Gen Int) `suchThat` (> 0)

randomSetQ :: Gen (Set Int)
randomSetQ = list2set <$> listOf posInt
-- / From Ex1.hs

interSet :: Set Int -> Set Int -> Set Int
interSet (Set []) set2 = Set []
interSet (Set (x:xs)) set2 | inSet x set2 = insertSet x rest
                           | otherwise = rest
                           where rest = interSet (Set xs) set2

unionSet' :: Set Int -> Set Int -> Set Int
unionSet' = unionSet

diffSet :: Set Int -> Set Int -> Set Int
diffSet (Set []) set2 = Set []
diffSet (Set (x:xs)) set2   | not $ inSet x set2 = insertSet x rest
                            | otherwise = rest
                            where rest = diffSet (Set xs) set2

-- Conversion from Set to List so functions such as all are available
set2list :: Ord a => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = x : set2list (Set xs)

-- Properties of what the result of the operations should be on the same set
prop_interSelf, prop_unionSelf, prop_diffSelf :: Set Int -> Bool
prop_interSelf s1 = interSet s1 s1 == s1
prop_unionSelf s1 = unionSet' s1 s1 == s1
prop_diffSelf s1 = diffSet s1 s1 == emptySet

-- Properties of what the operations should result in
prop_inter, prop_union, prop_diff :: Set Int -> Set Int -> Bool
prop_inter s1 s2 = all (\x -> inSet x s1 && inSet x s2) (set2list (interSet s1 s2))
prop_union s1 s2 = all (\x -> inSet x s1 || inSet x s2) (set2list (unionSet' s1 s2))
prop_diff s1 s2 = all (\x -> inSet x s1 && not (inSet x s2)) (set2list (diffSet s1 s2))

-- Function properties of the operations
prop_interAssoc, prop_unionAssoc, prop_diffUnion :: Set Int -> Set Int -> Set Int -> Bool
prop_interAssoc s1 s2 s3 = interSet s1 (interSet s2 s3) == interSet (interSet s1 s2) s3
prop_unionAssoc s1 s2 s3 = interSet s1 (interSet s2 s3) == interSet (interSet s1 s2) s3
prop_diffUnion  s1 s2 s3 = diffSet s3 (interSet s1 s2) == unionSet (diffSet s3 s1) (diffSet s3 s2) -- https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement

prop_interComm, prop_unionComm :: Set Int -> Set Int -> Bool
prop_interComm s1 s2 = interSet s1 s2 == interSet s2 s1
prop_unionComm s1 s2 = unionSet s1 s2 == unionSet s2 s1

-- Identity Relations
-- Note: The identity of the intersect is U which is infinite in case of natural numbers
-- This cannot be tested with a property such as diffId or unionId
prop_diffId, prop_unionId :: Set Int -> Bool
prop_diffId s1  =   diffSet s1 s1 == diffSet emptySet s1 &&
                    diffSet s1 s1 == emptySet
prop_unionId s1 =   unionSet s1 emptySet == s1


ex2 :: IO ()
ex2 = do
    -- Using the QuickCheck generator
    quickCheck $ forAll randomSetQ prop_inter
    quickCheck $ forAll randomSetQ prop_union
    quickCheck $ forAll randomSetQ prop_diff
    quickCheck $ forAll randomSetQ prop_interSelf
    quickCheck $ forAll randomSetQ prop_unionSelf
    quickCheck $ forAll randomSetQ prop_interAssoc
    quickCheck $ forAll randomSetQ prop_unionAssoc
    quickCheck $ forAll randomSetQ prop_diffUnion
    quickCheck $ forAll randomSetQ prop_interComm
    quickCheck $ forAll randomSetQ prop_unionComm
    quickCheck $ forAll randomSetQ prop_diffId
    quickCheck $ forAll randomSetQ prop_unionId

    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.