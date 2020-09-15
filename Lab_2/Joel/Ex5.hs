module Ex5 where

import Data.List
import Data.Char
import Data.Maybe (fromMaybe)
import System.Random
import Test.QuickCheck

rot :: String -> String
rot s = map addThirteen s

addThirteen :: Char -> Char
addThirteen c = case (lookup c $ transp ['a' .. 'z']) of
  Just x -> x
  Nothing -> fromMaybe c (lookup c $ transp ['A' .. 'Z'])

transp x = zip x ((drop 13 x) ++ (take 13 x))
