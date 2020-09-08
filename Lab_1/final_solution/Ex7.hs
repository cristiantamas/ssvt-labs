{- File: Ex7.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 7
-}

import Lab1
import Test.QuickCheck

splitDigits :: Integer -> [Integer]
splitDigits a | a < 1 = []
splitDigits 0 = []
splitDigits a = splitDigits (a `div` 10) ++ [a `mod` 10]

-- Reverse index a list: first element at index |list| and last at 0
reverseIndexed :: [a] -> [(Integer, a)]
reverseIndexed xs = reverse (zip [0 .. ] (reverse xs))

-- Double every other element starting from the first position left the check (last) digit
-- By reverse indexing the list we can multiply every element at uneven index
doubled :: [Integer] -> [Integer]
doubled xs =  map (\(i, x) -> if odd i then x * 2 else x) (reverseIndexed xs)

sumDigits :: [Integer] -> [Integer]
sumDigits xs = map (\x -> if x > 9 then x - 9 else x) (doubled xs)

luhn :: Integer -> Bool
luhn x = sum (sumDigits (splitDigits x)) `mod` 10 == 0

{-
 - American Express
-}
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n && americanExpressStart n && americanExpressLength n

-- Starts with 34 or 37
americanExpressStart :: Integer -> Bool
americanExpressStart n = x == 3 && (y == 4 || y == 7) where (x : y : _) = splitDigits n

americanExpressLength :: Integer -> Bool
americanExpressLength n = length (splitDigits n) == 15

{-
 - Mastercard
-}
isMaster :: Integer -> Bool
isMaster n = luhn n && mastercardStart n && mastercardLength n

-- Starts with 50 - 55
mastercardStart :: Integer -> Bool
mastercardStart n = x == 5 && (y `elem` [0 .. 5]) where (x : y : _) = splitDigits n

mastercardLength :: Integer -> Bool
mastercardLength n = length (splitDigits n) == 16

{-
 - Visa
-}
isVisa :: Integer -> Bool
isVisa n = luhn n && visaStart n && visaLength n

-- Starts with 4
visaStart :: Integer -> Bool
visaStart n = head (splitDigits n) == 4

-- Length of 16, sometimes 13
visaLength :: Integer -> Bool
visaLength n = ln == 16 || ln == 13 where ln = length (splitDigits n)

{-
 - Testing the Luhn Implementation:
 - Ideally we can generate Luhn numbers and test our implementation against it.
 - However, we have not been able to write such a function and thus we test all the
 - parts of our implementation seperately.
-}

-- Test for the calculation of the Check Digit
--  Check whether for every number you want to test
--  only one control digit is correct [0..9]
testCheckDigit :: Integer -> Bool
testCheckDigit n = length (filter luhn ([10 * n + x | x <- [0 .. 9]])) == 1

-- Helper function for the testing of the Split Digits function
-- Function that does the exact opposite as SplitDigits
reversedSplitDigits :: [Integer] -> Integer
reversedSplitDigits [] = 0
reversedSplitDigits [x] = x
reversedSplitDigits (x:xs) = x * (10 ^ length xs) + reversedSplitDigits xs

-- Testing the SplitDigit function
-- Using a self-written reverse of the function you want to test is not ideal,
-- but it does mitigate some risk as both must be wrong instead of only one.
testSplitDigits :: Integer -> Bool
testSplitDigits n = reversedSplitDigits (splitDigits n) == n

-- Testing the Sum Digits function by using the property that
-- not one of the Sum Digits should be greater than 9
testSumDigits :: [Integer] -> Bool
testSumDigits xs = not (any (> 9) (sumDigits xs))

main :: IO ()
main = do
  putStrLn "\n== Test Luhn algorithm =="
  quickCheckResult $ forAll genPositiveIntegers testCheckDigit

  putStrLn "\n== Test Split Digits Function =="
  quickCheckResult $ forAll genPositiveIntegers testSplitDigits

  putStrLn "\n== Test Sum Digits Function =="
  quickCheckResult $ forAll genSmallerThanTenList testSumDigits

  putStrLn ""