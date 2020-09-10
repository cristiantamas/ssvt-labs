
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
p --> q = not p || q || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

-- Ex 1 (1.5 hours)
naturals :: [Integer]
naturals = [1..]

-- Workshop 1.2
squarelst :: Integer -> Integer
squarelst n = foldr (\n acc ->  acc + n^2) 0 [1..n]

squarelst' :: Integer -> Integer
squarelst' n = div (n*(n+1)*(2*n+1)) 6

testSquarelst :: Integer -> Bool
testSquarelst n = squarelst n == squarelst' n

quickCheckSquarelst :: IO Result
quickCheckSquarelst = quickCheckResult $ forAll genPositiveIntegers testSquarelst

-- Workshop 1.3
cubelst :: Integer -> Integer
cubelst n = foldr (\x acc ->  acc + x^3) 0 [1..n]

cubelst' :: Integer -> Integer
cubelst' n = div (n ^ 2 + n) 2 ^ 2

testCubelst :: Integer -> Bool
testCubelst n = cubelst n == cubelst' n

quickCheckCubelst :: IO Result
quickCheckCubelst = quickCheckResult $ forAll genPositiveIntegers testCubelst

-- Ex 2 (30 minutes)
prop :: [Integer] -> Bool
prop a = 2 ^ length a == lengthPwrset a

lengthPwrset :: [Integer] -> Integer
lengthPwrset a = toInteger (length (subsequences a))

testPwrset :: [Integer] -> Bool
testPwrset = prop

-- Note: will run for a long time due to the exponential complexity
quickCheckPwrset :: IO ()
quickCheckPwrset = quickCheck testPwrset

-- Ex 3 (30 minutes)
lengthPerms :: [Integer] -> Integer
lengthPerms = product

lengthPerms' :: [Integer] -> Integer
lengthPerms' a = toInteger (length (permutations a))

testLengthPerms :: [Integer] -> Bool
testLengthPerms a = lengthPerms a == lengthPerms' a

quickCheckLengthPerms :: IO ()
quickCheckLengthPerms = quickCheck testLengthPerms

-- Ex 4 (20 minutes)
reversalPrimes :: [Integer]
reversalPrimes = filter (\x -> prime x && prime (reversal x)) [1..10000]

-- Ex 5 (10 minutes)
-- Returns 24680
-- Which is the sum of the first 101 primes
smallestPrime :: Integer
smallestPrime = sum (take 101 primes)

-- Ex 6 (30 minutes)
ex6Calculation :: Integer -> Integer
ex6Calculation n = product (take (fromIntegral n) primes) + 1

testEx6Calculation :: Integer -> Bool
testEx6Calculation n = not (prime (ex6Calculation n))

-- Smallest is 7
counterConjecture' :: [Integer]
counterConjecture' = filter testEx6Calculation primes

-- Ex 7 (1.5 hours)
-- Split integer into a list of its digits
digits :: Integer -> [Integer]
digits 0 = []
digits a = digits (a `div` 10) ++ [a `mod` 10]

-- reverse index a list: first element at index |list| and last at 0
reverseIndexed :: [b] -> [(Integer, b)]
reverseIndexed bs = reverse (zip [0 .. ] (reverse bs))

-- Double every other element starting from the first position left the check (last) digit
-- By reverse indexing the list we can multiply every element at uneven index
doubled :: [Integer] -> [Integer]
doubled xs =  map (\(i, x) -> if odd i then x * 2 else x) (reverseIndexed xs)

sumDigits :: [Integer] -> [Integer]
sumDigits xs = map (\x -> if x > 9 then x - 9 else x) (doubled xs)

luhn :: Integer -> Bool
luhn x = sum (sumDigits (digits x)) `mod` 10 == 0