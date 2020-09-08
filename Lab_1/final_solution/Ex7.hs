{- File: Ex7.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 1 Exercise 7
-}

-- TODO: Test for correctness

import Lab1

splitDigits :: Integer -> [Integer]
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
 - Luhn test
 - Part of testing this algorithm is check whether for every number you want to test
 - only one control digit is correct [0..9]
-}
testLuhn :: Integer -> Bool
testLuhn n = luhn n && length (filter luhn ([10 * n + x | x <- [0 .. 9]])) == 1

main :: IO ()
main = do
  putStrLn "\n== Test Luhn algorithm =="

  putStrLn ""