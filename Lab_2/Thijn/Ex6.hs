-- Lab 2 Exercises
-- Thijn Albers

import Lab2 ( Shape(..), (-->), probs )
import Data.List (intercalate, permutations, deleteBy, delete, nub)
import Test.QuickCheck
    ( listOf1,
      (==>),
      forAll,
      quickCheck,
      quickCheckWith,
      stdArgs,
      Arbitrary(arbitrary),
      Args(maxSize) )
import Text.Show.Functions ()
import Data.Char (chr, ord)


-- Ex6
-- Time: 1.5 hours

-- This is not a complete list, I have only implemented  the ones I needed for testing
-- Feel free to add the other 40 or so cases
lengthValid :: String -> Bool
lengthValid iban        | take 2 iban == ['N', 'L'] && length iban == 18 = True
                        | take 2 iban == ['G', 'B'] && length iban == 22 = True
                        | take 2 iban == ['S', 'E'] && length iban == 24 = True
                        | take 2 iban == ['C', 'R'] && length iban == 22 = True
                        | take 2 iban == ['B', 'G'] && length iban == 22 = True
                        | otherwise = False

moveCountry :: String -> String
moveCountry (c1:c2:c3:c4:cs) = cs ++ [c1, c2, c3, c4]

integerMapping :: Char -> String
integerMapping c        | ord c >= 65 && ord c <= 90 = show (ord c - 55)
                        | otherwise = [c]

ibanToInteger :: String -> Integer
ibanToInteger iban = read (intercalate "" (map integerMapping iban))

ibanValid :: String -> Bool
ibanValid iban =        lengthValid iban &&
                        ibanToInteger (moveCountry iban) `mod` 97 == 1

iban :: String -> Bool
iban i  | ' ' `notElem` i = ibanValid i
        | otherwise = iban (filter (/= ' ') i)


ex6 :: IO ()
ex6 = do
        -- Examples taken from https://www.iban.com/structure
        putStr "== Testing valid iban NL02ABNA0123456789 ==\n"
        print (iban "NL02ABNA0123456789")

        putStr "== Testing valid iban GB33BUKB20201555555555 ==\n"
        print (iban "GB33BUKB20201555555555")

        putStr "== Testing valid iban SE7280000810340009783242 ==\n"
        print (iban "SE7280000810340009783242")

        putStr "== Testing valid iban CR23015108410026012345 ==\n"
        print (iban "CR23015108410026012345")

        putStr "== Testing valid iban BG18RZBB91550123456789 ==\n"
        print (iban "BG18RZBB91550123456789")

        -- By changing numbers from the examples (in the bban or check digits)
        -- they are invalid. I have tested these to be invalid on https://transferwise.com/gb/iban/checker
        putStr "== Testing invalid iban NL51ABNA0495063945 ==\n"
        print (iban "NL51ABNA0495063945")

        putStr "== Testing invalid iban GB33BUKB21101492555505 ==\n"
        print (iban "GB33BUKB21101492555505")

        putStr "== Testing invalid iban CR23015108410026032039 ==\n"
        print (iban "CR23015108410026032039")

        putStr "== Testing valid iban CR02015108410026012345 ==\n"
        print (iban "CR02015108410026012345")

        putStr "== Testing invalid iban BG10RZBB91550123456789 ==\n"
        print (iban "BG10RZBB91550123456789")

        -- I have not been able to automate this testing


