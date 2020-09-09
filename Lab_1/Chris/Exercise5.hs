module Lab1 where
import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

get_sum_of_primes :: Integer
get_sum_of_primes = sum (take 101 primes)



main :: IO ()
main = do
    putStrLn "Sum of the first 101 primes"
    print(get_sum_of_primes)
    