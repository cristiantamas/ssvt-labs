{- File: Ex1.hs
 - Author: Leon Kielstra
 - Lab 2 Exercise 1 (Distribution)
-}

import Lab2 (probs)

toQuartile :: Float -> Integer
toQuartile n
  | n < 0.25 = 1
  | n < 0.5 = 2
  | n < 0.75 = 3
  | otherwise = 4

quartileList :: [Float] -> [Integer]
quartileList ns = map toQuartile ns

quartileListIO :: IO [Float] -> IO [Integer]
quartileListIO ns = fmap quartileList ns

countQuartile :: Integer -> [Integer] -> Int
countQuartile quartile ns = length (filter (== quartile) ns)

-- Running this multiple times shows that the claim seems correct
main :: IO ()
main = do
  ns <- quartileListIO (probs 10000)
  putStrLn "Quartile 1:"
  print (countQuartile 1 ns)
  putStrLn "Quartile 2:"
  print (countQuartile 2 ns)
  putStrLn "Quartile 3:"
  print (countQuartile 3 ns)
  putStrLn "Quartile 4:"
  print (countQuartile 4 ns)
