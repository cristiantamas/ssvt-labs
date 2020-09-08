module Lab1 where
import Data.List
import Test.QuickCheck 

-- Time spent: 20m
-- n! becomes bigger and bigger, making it harder to test 

no_of_permutations :: Integer -> Integer
no_of_permutations n = product[1..n]


no_of_permutations' :: [Integer] -> Integer
no_of_permutations' xs = toInteger(length(permutations xs))



test_no_of_permutations:: [Integer] -> Bool
test_no_of_permutations xs = no_of_permutations (toInteger(length xs)) == no_of_permutations' xs

main :: IO ()
main = do
    putStrLn "Proof induction Exercise 5"
    quickCheckWith stdArgs { maxSize = 12 } test_no_of_permutations