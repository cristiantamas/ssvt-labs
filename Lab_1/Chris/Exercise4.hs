module Lab1 where
import Data.List

-- Time spent: 20m

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

reversal :: Integer -> Integer
reversal = read . reverse . show


reversal_prime:: Integer -> Bool
reversal_prime n = prime n && prime ( reversal n)


main :: IO()
main = do
    let rev_primes = filter reversal_prime [1..10000]
    putStrLn "All reversal primes < 10000"
    print (rev_primes)
    