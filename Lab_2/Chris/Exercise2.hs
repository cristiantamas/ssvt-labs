import Lab2 ( Shape(..), (-->), probs )
import Data.List

-- Time spent: 30 min 

is_triangle :: Integer -> Integer -> Integer -> Bool
is_triangle a b c = (a + b > c)  && (a + c > b) && (b + c > a)

is_equilateral :: Integer -> Integer -> Integer -> Bool
is_equilateral a b c = (a == b) && (a == c)

is_isosceles :: Integer -> Integer -> Integer -> Bool
is_isosceles a b c = (a == b) || (a == c) || (b == c)

is_rectangular :: Integer -> Integer -> Integer -> Bool
is_rectangular a b c = (a * a + b * b == c * c ) || (a * a + c * c == b * b ) || (c * c + b * b == a * a )


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | not (is_triangle a b c) = NoTriangle
    | (is_triangle a b c) && (is_equilateral a b c) = Equilateral 
    | (is_triangle a b c) && (is_rectangular a b c) = Rectangular
    | (is_triangle a b c) && (is_isosceles a b c) = Isosceles
    | (is_triangle a b c) = Other
    | otherwise = Other

main :: IO()
main = do
    -- Other that trying different types of triangles, I don't know how to generate proper test cases
    print (triangle 9 15 12)
    print (triangle 6 6 6)
    print (triangle 6 5 14)
    print (triangle 6 6 5)
    print (triangle 6 5 4)
