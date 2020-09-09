module Lab1 where
import Data.List


prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

get_n_primes :: Integer -> [Integer]
get_n_primes n = take (fromIntegral n) primes


test_conjecture :: Integer -> Bool
test_onjecture n = prime (product (get_n_primes n) + 1)

refute_conjecture :: [Integer]
refute_conjecture = filter holds primes
    where holds n = not (testConjecture n)


main :: IO ()
main = do
    putStrLn "Smallest counter example to refute the conjecture"
    print(take 1 refute_conjecture)
    