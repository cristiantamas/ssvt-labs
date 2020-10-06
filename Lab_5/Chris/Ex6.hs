-- Lab 5
-- Exercise 6
-- Time: 2 hr

-- We can create the set of pairs (x,y) and then check if the length of the set is equal to the number of
-- nodes in the grown tree. This is a pre-condition to ensure that the three will match the set
-- Then we can use the function collect to convert the tree into a list of pairs, just like the set
-- we created above. We parse the set and verify each element against the elements of the set
-- we created
-- If we find a matching element, then we go to the next one from the set until we reached the end
-- If we don't find a match, then the check stops
-- This is a blunt approach and I feel that there might be better ways to do it. Also, I didn't check 
-- if there is any posibility to have duplicates in the tree, case in which we cannot match the
-- length of the tree to the length of the set (sets don't have duplicates)

import Data.List
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Lecture5

tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

get_set:: Integer -> Integer -> Integer -> [(Integer, Integer)]
get_set i j n 
    | i == n && j == (n + 1) = [(1,1)]
    | i == (n + 1) && j /= (n + 1) = get_set 1 (j + 1) n
    | i /= (n + 1) && j == (n + 1) = get_set (i + 1) 1 n
    | coprime i j == True =   (i, j):(get_set i (j + 1) n)
    | otherwise = get_set i (j + 1) n

collect :: Tree a -> [a]
collect (T x ts) = x : concatMap collect ts

check_element :: [(Integer, Integer)] -> (Integer, Integer) -> Bool
check_element xs (a,b) = elem (a,b) xs

check_sets :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
check_sets xs [] = True
check_sets xs (x:ys) = (check_element xs x) && check_sets xs ys

--main :: IO()
--main = do
--    print $ check_sets ( collect (tree1 3) ) (get_set 1 2 3)


