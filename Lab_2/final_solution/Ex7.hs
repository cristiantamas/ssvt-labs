{- File: Ex7.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 2 Exercise 7 (Implementing and testing IBAN validation)
-}

import Data.Char (ord)
import Data.List

validLength :: String -> Bool
validLength str
  | country == "NL" && length str == 18 = True
  | country == "GB" && length str == 22 = True
  | country == "SE" && length str == 24 = True
  | country == "CR" && length str == 22 = True
  | country == "BG" && length str == 22 = True
  | otherwise = False
  where
    country = take 2 str

swap4chars :: String -> String
swap4chars str = let (x, y) = (drop 4 str, take 4 str) in x ++ y

toInt :: Char -> String
toInt c
  | elem c ['A' .. 'Z'] = show (ord c - 55)
  | elem c ['0' .. '9'] = [c]
  | otherwise = error "Invalid character"

mapStringToInt :: String -> String -> Integer
mapStringToInt "" out = read out
mapStringToInt (c : str) out = mapStringToInt str (out ++ toInt c)

iban :: String -> Bool
iban str = validLength escaped && (mapStringToInt (swap4chars escaped) "") `mod` 97 == 1
  where
    escaped = filter (/= ' ') str

{-
 - One way to test this is to plot in a list of examples IBANs as well as invalid ones.
 - You could also generate random numbers and calculate the check digits. Then use both those
 - generate numbers as well as ones with invalid check digits. The downside of this is that it uses
 - a lot of functions that are also used by the IBAN check function. s
-}
ex7 :: IO ()
ex7 = do
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
