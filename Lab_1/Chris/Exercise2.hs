module Lab1 where
import Data.List
import Test.QuickCheck 

-- Time spent: 20m
 -- Workshop 1, Exercise 4 
power_set_length :: [Integer] -> Int
power_set_length xs = length(subsequences xs)


power_set_length_test :: [Integer] -> Bool
power_set_length_test xs = power_set_length xs == 2 ^ length(xs)


main :: IO()
main = do
    putStrLn "Proof induction Exercise 4"
    quickCheckWith stdArgs { maxSize = 32 } power_set_length_test
    