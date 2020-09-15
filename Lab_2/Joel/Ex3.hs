module Ex3 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{-
a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10].

b) Provide a descending strength list of all the implemented properties.

Workshop 3:

Which of the following properties is stronger, left side or right side? assume
domain [1..10]
• (\ x -> even x && x > 3) or even
• (\ x -> even x || x > 3) or even
• (\ x -> (even x && x > 3) || even x) or even
• even or (\ x -> (even x && x > 3) || even x)



TIME: 1 hour 30 minutes
-}

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- Didn't understand how to implenet this ^

propOne, propTwo, propThree, propFour :: Int -> Bool
propOne n = (even n && n>3) || even n
propTwo n = (even n || n>3) || even n
propThree n = ((even n && n > 3) || even n) || even n
propFour n = even n || ((even n && n > 3) || even n)


strPropOne, strPropTwo, strPropThree, strPropFour :: Bool -> Int
strPropOne n = length (filter (\x -> x == n) (map propOne [(-10) .. 10]))
strPropTwo n =  length (filter (\x -> x == n) (map propTwo [(-10) .. 10]))
strPropThree n =  length (filter (\x -> x == n) (map propThree [(-10) .. 10]))
strPropFour n =  length (filter (\x -> x == n) (map propFour [(-10) .. 10]))


{-Results: The lower the amount of Trues in the bool list, the stronger the condition.
           The length str functions above filter the all the Trues and then calculates the lenght of the list.

           strPropOne = 11
           strPropThree = 11
           strPropFour = 11
           strPropTwo = 14

           So strPropOne, strPropThree and strPropFour have the same condition strength, while strPropThree is weaker
           since this property has more Trues. -}

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation m l = all (\x -> elem x m) l && all (\x -> elem x l) m


testPermOne, testPermTwo, testPermThree, testPermFour, testPermFive, testPermSix :: Bool
testPermOne = isPermutation ['a', 'b', 'c'] ['c', 'b', 'a'] == True
testPermTwo = isPermutation [1,2,3] [3,2,1] == True
testPermThree = isPermutation ["String 1", "String 2"] ["String 2", "String 1"] == True
testPermFour = isPermutation [True, False, True, False, True] [True, False, True, False] == False
testPermFive = isPermutation [True, False, True, False, True] [True, False, True, False, False] == False
testPermSix = isPermutation [True, False, True, False, True] [True, False, True, False, True] == False


-- isPermutation strPropOne, strPropTwo = (strPropOne True == strPropTwo True) && (strPropOne True == strPropTwo True) &&
