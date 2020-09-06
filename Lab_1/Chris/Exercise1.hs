module Lab1 where
import Test.QuickCheck 

-- Time spent: 1h
-- Workshop 1, Exercise 2

square_function :: Integer -> Integer
square_function x = x * x 

sum_of_squares' :: Integer -> Integer
sum_of_squares' n = sum(map square_function [1..n])

sum_of_squares :: Integer -> Integer
sum_of_squares n = (n * (n + 1) * (2 * n + 1)) `div` 6

test_sum_of_squares :: Integer -> Bool
test_sum_of_squares n = let a = abs n in sum_of_squares' a == sum_of_squares a 



-- Workshop 1, Exervise 3

cube_function :: Integer -> Integer
cube_function x = x * x * x 

sum_of_cubes' :: Integer -> Integer
sum_of_cubes' n = sum(map cube_function [1..n])

sum_of_cubes :: Integer -> Integer
sum_of_cubes n = square_function (n * (n + 1) `div` 2)

test_sum_of_cubes :: Integer -> Bool
test_sum_of_cubes n = let a = abs n in sum_of_cubes' a == sum_of_cubes a 


-- Main 

main :: IO()
main = do
    putStrLn "Proof induction Exercise 2"
    quickCheck test_sum_of_squares

    putStrLn "Proof induction Exervise 3 "
    quickCheck test_sum_of_cubes