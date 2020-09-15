module Ex6 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

numDigits :: Int -> Int
numDigits n = length $ show n
-- toDigits :: Int -> [Int]
-- toDigits = map digitToInt . show


-- iban :: Int -> Bool
-- iban n = (mod n == 1) && (numDigits n == 28)

iban :: Int -> Bool
iban n = ((mod n 97) == 1)
