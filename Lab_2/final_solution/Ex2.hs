{- File: Ex2.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 2 Exercise 2 (Triangles)
-}

import Data.List (permutations, sort)
import Test.QuickCheck ()

data Shape
  = NoTriangle
  | Equilateral
  | Isosceles
  | Rectangular
  | Other
  deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = triangleType (sort [x, y, z])

triangleType :: [Integer] -> Shape
triangleType [x, y, z]
  | x + y <= z = NoTriangle
  | x == y && x == z = Equilateral
  | x ^ 2 + y ^ 2 == z ^ 2 = Rectangular
  | x == y || y == z = Isosceles
  | otherwise = Other

{-
 - Tests
 - We can write tests with input that we are sure of that they satisfy a particular type of triangle.
 - We also have to show that the order of the input does not matter.
-}

testTriangle :: Shape -> [[Integer]] -> Bool
testTriangle shape testData = filter (\[x, y, z] -> (triangle x y z) /= shape) testData == []

-- For debugging purposes
testTriangle' :: Shape -> [[Integer]] -> [[Integer]]
testTriangle' shape testData = filter (\[x, y, z] -> (triangle x y z) /= shape) testData

-- Generate test cases for NoTriangle
genNoTriangle :: [[Integer]]
genNoTriangle = [[x, y, z] | x <- [-5 .. 10], y <- [x .. 10], z <- [x + y + 1]]

testNoTriangle :: Bool
testNoTriangle = testTriangle NoTriangle genNoTriangle

-- Generate test cases for Equilateral
genEquilateral :: [[Integer]]
genEquilateral = [[x, y, z] | x <- [1 .. 100], y <- [x], z <- [x]]

testEquilateral :: Bool
testEquilateral = testTriangle Equilateral genEquilateral

-- Generate test cases for Rectangular
genRectangular :: [[Integer]]
genRectangular = map (\a -> map (* a) [3, 4, 5]) [1 ..]

testRectangular :: Bool
testRectangular = testTriangle Rectangular (take 100 genRectangular)

-- Generate test cases for Rectangular
-- Equilateral is a stronger property which we have already tested, so no need to do so here.
genIsosceles :: [[Integer]]
genIsosceles = [[x, y, z] | x <- [2 .. 100], y <- [x], z <- [x -1]]

testIsosceles :: Bool
testIsosceles = testTriangle Isosceles genIsosceles

-- Multiple inputs with parameters in different order
genInput :: [[Integer]]
genInput = [[x, y, z] | x <- [-1 .. 10], y <- [-1 .. 10], z <- [-1 .. 10]]

testSequences :: Bool
testSequences = filter (\sides -> not (testSequence sides)) genInput == []

testSequence :: [Integer] -> Bool
testSequence xs = length (unique (map (\[x, y, z] -> triangle x y z) (permutations xs))) == 1

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter ((/=) x) xs)

ex2 :: IO ()
ex2 = do
  putStrLn "No triangles:"
  print testNoTriangle

  putStrLn "No equiliteral:"
  print testEquilateral

  putStrLn "No rectangular:"
  print testRectangular

  putStrLn "No isosceles:"
  print testIsosceles

  putStrLn "No parameter order:"
  print testSequences

  putStrLn ""
