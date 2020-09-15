import Lab2 ( Shape(..), (-->), probs )
import Data.List

-- Time spent: 2h 30min


distibution :: Float -> String
distibution n 
    | n > 0 && n < 0.25 = "Q1"
    | n >= 0.25 && n < 0.5 = "Q2"
    | n >= 0.5 && n < 0.75 = "Q3"
    | n >= 0.75 && n < 1 = "Q4"

find_quarters :: [Float] -> [String]
find_quarters = map distibution

find_quarters_monad :: IO [Float] -> IO [String]
find_quarters_monad = fmap find_quarters

count_quarters :: [String] -> String -> Int
count_quarters xs s = length (filter (== s) xs)

main :: IO()
main = do
    generated <- find_quarters_monad (probs 10000)
    putStrLn "Distribution"
    putStr "Q1= "
    print (count_quarters generated "Q1")
    putStr "Q2= "
    print (count_quarters generated "Q2")
    putStr "Q3= "
    print (count_quarters generated "Q3")
    putStr "Q4= "
    print (count_quarters generated "Q4")
    