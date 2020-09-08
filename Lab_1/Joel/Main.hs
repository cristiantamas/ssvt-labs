

module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

naturals :: [Integer]
naturals = [1 ..]


-- Ex1. Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.

-- Time: 1 hour.

-- 1.2:
squareSum1 :: Integer -> Integer
squareSum1 n = div (n *(n+1)*(2*n+1)) 6

squareSum2 :: Integer -> Integer
squareSum2 n = sum (map (^2) [1..n])

checkInductionSq :: Integer -> Bool
checkInductionSq n = squareSum1 n == squareSum2 n


-- 1.3
cubeSum1 :: Integer -> Integer
cubeSum1 n = div ((n * (n+1))^2) (2^2)

cubeSum2 :: Integer -> Integer
cubeSum2 n = sum (map (^3) [1..n])

checkInductionCube :: Integer -> Bool
checkInductionCube n = cubeSum1 n == cubeSum2 n

--Ex2.

-- Time: 45 min.

checkLength1 :: [Int] -> Int
checkLength1 n = length (subsequences n)

checkLength2 :: [Int] -> Int
checkLength2 n = 2^(length n)

checkInductionEx2 :: [Int] -> Bool
checkInductionEx2 n = checkLength1 n == checkLength2 n

--Ex4. The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

--Time: 40 min

revPrime :: [Integer]
revPrime = filter (\x -> prime x && prime (reversal x)) [1..10000]

-- getPrime :: [Integer]
-- getPrime = filter prime [1..10000]
--
-- getRevPrime :: [Integer]
-- getRevPrime = filter getPrime



--Ex5. The number 101 is a prime, and it is also the sum of five consecutive primes, namely 13+17+19+23+29. Find the smallest prime number that is a sum of 101 consecutive primes.

-- Time: 15 min

-- sumPrimes :: Integer
-- sumPrimes = sum(filter prime [1..101]) -- The sum of 101 consecutive primes is 1161

sumPrimes :: Integer
sumPrimes = sum(take 101 primes)

--Ex6.

--Time: 30 min

is_prime :: Integer -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
		   | otherwise = True

consecutivePrimes :: Int -> [Integer]
consecutivePrimes n = take n primes

prodConPrimes :: Int -> Integer
prodConPrimes n = (product (consecutivePrimes n)) + 1

refuteConjecture :: Int -> Bool
refuteConjecture n = is_prime (prodConPrimes n)

--Ex 7:
