-- Lab 5
-- Exercise 4
-- Time: 2 hr

import Data.List
import Test.QuickCheck
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Lecture5


carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
       k <- [2..],
       prime (6*k+1),
       prime (12*k+1),
       prime (18*k+1) ]


-- Ex3 begins

-- This function takes the a list of composites numbers to be tested agains Fermat's theorem and a
-- k that represents the number of tests
-- When the first number that breaks the function is found, it is returned
test_fermat :: [Integer] -> Int -> Integer
test_fermat [] k = -1 
test_fermat (x:comps) k 
    | unsafePerformIO (primeTestsF k x) = x 
    | otherwise = test_fermat comps k

-- Ex3 ends


test_miller_rabin :: [Integer] -> Int -> Integer
test_miller_rabin [] k = -1
test_miller_rabin (x:xs) k
    | unsafePerformIO (primeMR k x) = x 
    | otherwise = test_miller_rabin xs k


--main:: IO()
--main = do

--    putStrLn "Fermat's primality test"

--    putStr "For k = 1: "
--    print $ test_fermat carmichael 1

--    putStr "For k = 2: "
--    print $ test_fermat carmichael 2

--    putStr "For k = 3: "
--    print $ test_fermat carmichael 3
    
--    putStr "For k = 4: "
--    print $ test_fermat carmichael 4

--    putStr "For k = 7: "
--    print $ test_fermat carmichael 7

--    putStr "For k = 10: "
--    print $ test_fermat carmichael 10

--    putStrLn "Miller-Rabin primality test"

--    putStr "For k = 1: "
--    print $ test_miller_rabin carmichael 1

--    putStr "For k = 2: "
--    print $ test_miller_rabin carmichael 2



-- After testing a few times, I saw that the first generated Carmichael number breaks Fermat's primality check.
-- But that is ok, since Charmichael numbers are composite numbers that have the property of Fermat's little theorem
-- (if p is a prime number, then for any integer b, the number b^p − b is an integer multiple of p)
-- In this case, they break Fermat's primality check. They are also called Fermat pseudoprimes or absolute Fermat pseudoprimes
-- Each Charmicael number will pass Fermat's primality check
-- However, if Charmicael numbers are teste with Euler's formula or with a strong probable prime test, they will be proven to be composite



-- Test case 1 - Fermat's
-- For k = 1: 294409
-- For k = 2: 294409
-- For k = 3: 294409
-- For k = 4: 294409

-- Test case 2 - Fermat's
-- For k = 1: 294409
-- For k = 2: 294409
-- For k = 3: 294409
-- For k = 4: 294409
-- For k = 7: 294409
-- For k = 10: 294409


-- My testing couldn't found any Charmicael number to break the Miller-Rabin's primality check.
-- Since they improved the algortihm using Extended Riemann Hypothesis and factoring out powers of 2,
-- I strongly believed they ruled out Charmicael numbers from breaking their test

-- Test case 3 - Miller-Rabin
-- For k = 1: nothing found 
