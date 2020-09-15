{- File: Ex6.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 2 Exercise 6 (Implementing and testing ROT13 encoding)
-}

import Data.Char (chr, ord)
import Data.List (delete, deleteBy, nub, permutations)
import Lab2 (Shape (..), probs, (-->))
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Args (maxSize),
    forAll,
    listOf1,
    quickCheck,
    quickCheckWith,
    stdArgs,
    (==>),
  )
import Text.Show.Functions ()

-- Ex 5

-- Specification:
-- E (x) = (x + 13) mod 26
-- Because of the shift by 13 and the alphabet containing 26 letters,
-- decryption of x is encrypting it twice.
-- D (x) = E (E (x))

shiftCapital :: Char -> Int -> Char
shiftCapital c s = chr ((((ord c - 65) + s) `mod` 26) + 65)

shiftSmall :: Char -> Int -> Char
shiftSmall c s = chr ((((ord c - 97) + s) `mod` 26) + 97)

ceaser :: Char -> Int -> Char
ceaser c s
  | ord c >= 65 && ord c <= 90 = shiftCapital c s
  | ord c >= 97 && ord c <= 122 = shiftSmall c s
  | otherwise = c

rot13 :: String -> String
rot13 = map (`ceaser` 13)

-- Decryption is equal to encrypting twice
prop_rotDecrypt :: String -> Bool
prop_rotDecrypt cs = rot13 (rot13 cs) == cs

-- Testing if letters map to the correct one
-- Upper / lower case shifts
-- Cyclic or not
-- Anything not in the alphabet should not change
prop_shift, prop_shift', prop_shift'', prop_shift''', prop_shift'''', prop_shift''''' :: String -> Bool
prop_shift cs = map (== 'j') cs == map (== 'w') (rot13 cs)
prop_shift' cs = map (== 'J') cs == map (== 'W') (rot13 cs)
prop_shift'' cs = map (== 'X') cs == map (== 'K') (rot13 cs)
prop_shift''' cs = map (== 'x') cs == map (== 'k') (rot13 cs)
prop_shift'''' cs = map (== '#') cs == map (== '#') (rot13 cs)
prop_shift''''' cs = map (== '-') cs == map (== '-') (rot13 cs)

ex6 :: IO ()
ex6 = do
  quickCheck prop_rotDecrypt
  quickCheck prop_shift
  quickCheck prop_shift'
  quickCheck prop_shift''
  quickCheck prop_shift'''
  quickCheck prop_shift''''
  quickCheck prop_shift'''''
