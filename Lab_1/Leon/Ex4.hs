{- File: Ex4.hs
 - Author: Leon Kielstra
 - Lab 1 Exercise 4
-}
import Lab1

reversePrimes :: [Integer]
reversePrimes = filter (\x -> reversePrime x) [1 .. 10000]

reversePrime :: Integer -> Bool
reversePrime n = prime n && prime (reversal n)
