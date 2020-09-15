module Ex2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


{--Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:

- Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,

- Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,

- Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,

- Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,

- Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles. --}

--Time: 45 min

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z =
  if isNoTriangle x y z then NoTriangle
  else if isEquilateral x y z then Equilateral
  else if isRectangular x y z then Rectangular
  else if isIsosceles x y z then Isosceles
  else Other

isNoTriangle, isEquilateral, isRectangular, isIsosceles :: Integer -> Integer -> Integer -> Bool
isNoTriangle x y z = (x + y < z || z + y < x || x + z < y)
isEquilateral x y z = x==y && x==z
isRectangular x y z = ((x^2) + (y^2) == (z^2) || (x^2) + (z^2) == (y^2) || (z^2) + (y^2) == (x^2))
isIsosceles x y z = (x/=y && x/=z && y==z)

--TODO∷Implement test

test_Equilateral :: Integer -> Integer -> Integer -> Bool
test_Equilateral x y z = triangle x y z == Equilateral

test_NoTriangle :: Integer -> Integer -> Integer -> Bool
test_NoTriangle x y z = triangle x y z == NoTriangle

test_Isosceles :: Integer -> Integer -> Integer -> Bool
test_Isosceles x y z = triangle x y z == Isosceles

test_Rectangular :: Integer -> Integer -> Integer -> Bool
test_Rectangular x y z = triangle x y z == Rectangular

ex2 :: IO ()
ex2 = do

  print(test_Equilateral 3 3 3)
  print(test_NoTriangle 1 2 7)
  print(test_Isosceles 8 6 6)
  print(test_Rectangular 12 9 15)
