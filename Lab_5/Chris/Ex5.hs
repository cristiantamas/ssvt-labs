-- Lab 5
-- Exercise 5
-- Time: 1 hr
-- Mersenne primes are numbers of form m = 2^n - 1 for some integer n that is prime


import Data.List
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Lecture5


-- The function takes a list of prime integers and returns the list
-- of Mersenne numbers found using those primes
-- We take a prime x, and compute 2^x - 1 and check if it is a prime 
-- If it passes the check, then it is a Marsenne prime and we add it to the list
-- For checking if 2^x - 1 are prime, we use Miller-Rabin primality check with k = 5 number of candidates
check_marsenne :: [Integer] -> [Integer]
check_marsenne (x:xs)
    | unsafePerformIO(primeMR 5 (2^x - 1)) = (2^x - 1):(check_marsenne xs)
    | otherwise = check_marsenne xs


--main:: IO()
--main = do
--    print $ check_marsenne primes


-- Output:
-- [3,7,31,127,8191,131071,524287]

-- The output stops at 524287, because the next Marsenne prime is 2147483647, which is too big to 
-- be saved into an Integer type