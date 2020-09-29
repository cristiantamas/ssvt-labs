-- Lab 4.4
-- Time: 3hrs so far
-- Author: Thijn Albers

-- TODO: Another serial property and answer question

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

-- possible properties of serial relations to test
-- R is serial -> for any S: (complement of union r. s) = subset of (r . complement s)
-- R is serial -> complement (r . empty relation) = empty set
-- R is serial -> Identity subset of R. Rt
-- R is serial -> Complement of R is subset of R . Identity

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

prop_isSerial2 :: Rel Int -> Bool
prop_isSerial2 r = isSerial (domR r) r --> True

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
    -- We should also test for other functions as we are now only verifying
    -- serial can spot some reflexive functions
    quickCheck $ forAll genReflexive prop_isSerial1
    quickCheck $ forAll genReflexive' prop_isSerial1

    -- Consider the relation R = {(x, y) | x = y(mod n)},
    -- where (mod n) is the modulo function in modular arithmetic and n > 0.
    -- Discuss whether (and when) R is serial. How can you test whether R is serial?
    -- How can you prove that R is serial?

    -- +++ OK, passed 100 tests.
    -- +++ OK, passed 100 tests.