{- File: Ex2.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 3 Exercise 2 
-}
-- Time spent: 1.5h

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import GHC.Base (liftM2, liftM)
import Generators

allEvls :: Form -> [Bool]
allEvls f = map (`evl` f) (allVals f)

equiv :: Form -> Form -> Bool
equiv f1 f2 = allEvls f1 == allEvls f2


-- To test the parser we created a Form generator. By doing this we could test our properties
-- on a large number of cases.
-- The first property is that for a string representation of a form (show),
-- the parser should return a form which is equivalent to the original form
-- As we have tested the equiv function above, we can use it in our property

prop_ParseShow :: Form -> Bool
prop_ParseShow f = equiv f ( head (parse (show f)))

-- Output:
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
ex2 :: IO ()
ex2 = do
        quickCheckWith stdArgs { maxSize = 5 } $ forAll form prop_ParseShow
        quickCheckWith stdArgs { maxSize = 10 } $ forAll form prop_ParseShow
        quickCheckWith stdArgs { maxSize = 15 } $ forAll form prop_ParseShow