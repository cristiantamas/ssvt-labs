-- Lab 4
-- Exercise 4

module Ex4 where

import Data.List
import SetOrd
import Test.QuickCheck
import Control.Monad

type Rel a = [(a,a)]

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial a r = all (\x -> any (\y -> (x, y) `elem` r) a) a

-- Generae a relation with positive integers
relInt :: Gen Int
relInt = abs `fmap` (arbitrary:: Gen Int) `suchThat` (> 0)

relPair :: Gen (Int, Int)
relPair = liftM2 (,) relInt relInt

genRelInt :: Gen (Rel Int)
genRelInt = listOf1 relPair

identityR :: [Int] -> Rel Int
identityR d = [(x, x) | x <- d]

-- Source: Haskell Road 5.3
domR :: Rel Int -> [Int]
domR r = [ x | (x,_) <- r ]

prop_isSerial1 :: Rel Int ->  Bool
prop_isSerial1 r = isSerial (domR r) r

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- There are a number of possible properties of serial relations to test:
-- R is serial -> for any S: (complement of union r. s) = subset of (r . complement s)
-- R is serial -> complement (r . empty relation) = empty set
-- R is serial -> Identity subset of R. Rt
-- R is serial -> Complement of R is subset of R . Identity
--
-- We have not been able to implement any of these properties, as they require the implication of R is serial
-- To random test these properties, we would have to be able to generate serial functions because the chance that
-- a randomly generated relation is serial is small. Testing these properties with random relations would result
-- in many trivial tests (because False -> anyting == True)
-- (under the assumption that isSerial works ofcourse)
-- quickCheck prop_noSerial gives a trivial score between 90-95%
prop_noSerial :: Rel Int -> Property
prop_noSerial r = classify (not $ isSerial (domR r) r) "trivial" True

-- Generating the reflexive / serial relations 'divisor' and 'equal'
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

relDivisors :: Int -> Rel Int
relDivisors n = [(n, d) | d <- divisors n]

genReflexive, genReflexive' :: Gen (Rel Int)
genReflexive = (arbitrary :: Gen (Rel Int)) `suchThat` all (uncurry (==))
genReflexive' = relDivisors <$> (arbitrary :: Gen Int)


ex4 :: IO ()
ex4 = do
    -- All reflexive functions are serial, so we can use them to test our serial check
    -- We would like to test for other functions as well, because  we are now only verifying
    -- serial can spot reflexive functions.
    quickCheck $ forAll genReflexive prop_isSerial1 -- +++ OK, Passed 100 tests.
    quickCheck $ forAll genReflexive' prop_isSerial1 -- +++ OK, Passed 100 tests.