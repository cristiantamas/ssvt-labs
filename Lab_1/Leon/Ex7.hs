{- File: Ex7.hs
 - Author: Leon Kielstra
 - Lab 1 Exercise 7
-}

import Test.QuickCheck (Gen, arbitrary, elements, quickCheckResult)

luhn :: Integer -> Bool
luhn n = sum (doubleEveryOther (splitDigits n)) `mod` 10 == 0

splitDigits :: Integer -> [Integer]
splitDigits 0 = []
splitDigits n = splitDigits (div n 10) ++ [mod n 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : y : zs) = x : luhnDouble y : doubleEveryOther zs

luhnDouble :: Integer -> Integer
luhnDouble x
  | x < 5 = 2 * x
  | x > 4 = 2 * x - 9

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
mastercardStart n = x == 5 && (elem y [0 .. 5]) where (x : y : _) = splitDigits n

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
 - Luhn test
 - Part of testing this algorithm is check whether for every number you want to test
 - only one control digit is correct [0..9]
-}
testLuhn :: Integer -> Bool
testLuhn n = length (filter luhn ([10 * n + x | x <- [0 .. 9]])) == 1
