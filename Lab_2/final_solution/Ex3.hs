{- File: Ex3.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 2 Exercise 3 (Testing properties strength)
-}

import Data.List (sortBy)
import Lab2 ((-->))

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

prop_1 :: Int -> Bool
prop_1 x = even x && x > 3

prop_2 :: Int -> Bool
prop_2 x = even x || x > 3

prop_3 :: Int -> Bool
prop_3 x = (even x && x > 3) || even x

prop_4 :: Int -> Bool
prop_4 x = prop_3 x

checkProp :: String -> Int -> Bool
checkProp prop x
  | prop == "prop_1" = prop_1 x
  | prop == "prop_2" = prop_2 x
  | prop == "prop_3" = prop_3 x
  | prop == "prop_4" = prop_4 x
  | prop == "even" = even x

propertySort :: String -> String -> Ordering
propertySort a b
  | weaker [-10 .. 10] (checkProp a) (checkProp b) = GT
  | otherwise = LT

customSort :: [String]
customSort = sortBy propertySort ["prop_1", "prop_2", "prop_3", "prop_4", "even"]

ex3 :: IO ()
ex3 = do
  putStrLn "Properties strongest first"
  print customSort
