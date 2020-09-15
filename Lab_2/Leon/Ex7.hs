{- File: Ex7.hs
 - Author: Leon Kielstra
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
  | otherwise = False
    where country = take 2 str

swap4chars :: String -> String
swap4chars str = let (x,y) = (drop 4 str, take 4 str) in x++y

toInt :: Char -> String
toInt c
  | elem c ['A'..'Z'] = show (ord c - 55)
  | elem c ['0'..'9'] = [c]
  | otherwise = error "Invalid character"

mapStringToInt :: String -> String -> Integer
mapStringToInt "" out = read out
mapStringToInt (c:str) out = mapStringToInt str (out ++ toInt c)

iban :: String -> Bool
iban str = validLength str && (mapStringToInt (swap4chars str) "") `mod` 97 == 1

{-
 - One way to test this is to plot in a list of examples IBANs as well as invalid ones.
 - You could also generate random numbers and calculate the check digits. Then use both those
 - generate numbers as well as ones with invalid check digits. The downside of this is that it uses
 - a lot of functions that are also used by the IBAN check function. s
-}
