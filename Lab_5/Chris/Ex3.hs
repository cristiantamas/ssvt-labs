-- Lab 5
-- Exercise 3
-- Time: 1 hr

import Data.List
import Test.QuickCheck
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Lecture5


-- Ex2 begins

-- The function checks all the numbers between x, where 2 <= x < n
-- We get a list with results for mrComposite for all the numbers in the interval
-- If we can find some results that are true, then n is a composite 
is_composite:: Integer -> Bool
is_composite n = (takeWhile (\x -> mrComposite x n == True) [2..n-1]) /= []


-- Composite number generator
-- For testing puroses and to not crash anything, this was tested the interval 1 .. 100
-- The results are below
composites' :: [Integer]
composites' = filter (\x -> is_composite x == True) [1..]

-- Ex2 ends


-- This function takes the a list of composites numbers to be tested agains Fermat's theorem and a
-- k that represents the number of tests
-- When the first number that breaks the function is found, it is returned
test_fermat :: [Integer] -> Int -> Integer
test_fermat [] k = -1 
test_fermat (x:comps) k 
    | unsafePerformIO (primeTestsF k x) = x 
    | otherwise = test_fermat comps k


--main:: IO()
--main = do
--    putStr "For k = 1: "
--    print $ test_fermat composites' 1

--    putStr "For k = 2: "
--    print $ test_fermat composites' 2

--    putStr "For k = 3: "
--    print $ test_fermat composites' 3
    
--    putStr "For k = 4: "
--    print $ test_fermat composites' 4

--    putStr "For k = 5: "
--    print $ test_fermat composites' 5

--    putStr "For k = 7: "
--    print $ test_fermat composites' 7


-- The testing was done on a treshold of composite numbers between 1 and 10000 at first
-- However, if I break the upper limit and just let the tests flow, it becomes really hard to find
-- something. 
-- The below shown test is a case in which the random numbers for testing were picked favourably to
-- find something. However, I got situations in which the program continously runs for k = 7 tests, since as you increase the number of tests
-- it becomes harder to find a composite number that breaks Fermat's theorem
--
-- Another worth to metion fact is that sometimes for a lower number of tests you find a bigger number than
-- for a bigger number of tests. That is owed to the randomness of picking sequences of numbers

-- Test case 1
-- For k = 1: 45
-- For k = 2: 65
-- For k = 3: 1105
-- For k = 4: 561
-- For k = 5: 1729
-- For k = 7: 8911

-- Test case 2
-- For k = 1: 91
-- For k = 2: 91
-- For k = 3: 1105
-- For k = 4: 6601
-- For k = 5: 10585
-- For k = 7: 1105


