-- Lab3 exercises
-- Thijn Albers

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import GHC.Base (liftM2, liftM)

-- Ex2
-- Time: 1.5 hours

equiv :: Form -> Form -> Bool
equiv f1 f2 = allEvls f1 == allEvls f2

allEvls :: Form -> [Bool]
allEvls f = map (`evl` f) (allVals f)

-- To test the parser I created a Form generator. By doing this we can test our properties
-- on a large number of cases.
-- The first property is that for a string representation of a form (show),
-- the parser should return a form which is equivalent to the original form
-- As we have tested the equiv function above, we can use it in our property

form :: Gen Form
form = sized form'

instance Arbitrary Form where
    arbitrary = form

-- arbitrary or choose in range?
name :: Gen Name
name = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

name2 :: Gen Name
name2 = choose (0,10)

-- The generation of forms is recursive and the size of a form generated can quickly grow
-- This makes the generation very computationally intensive
-- To counter this we set a maximum recursion level of 5, which still results in large forms
-- but computationally doable.
form' :: Integral a => a -> Gen Form
form' 0 = fmap Prop name2
form' n | n > 0 = oneof [fmap Prop name2,
              fmap Neg subform,
              Cnj <$> listOf subform,
              Dsj <$> listOf subform,
              liftM2 Impl subform subform,
              liftM2 Equiv subform subform]
              where subform = form' (n `div` 2)

sampleForm :: IO ()
sampleForm = sample form

prop_ParseShow :: Form -> Bool
prop_ParseShow f = equiv f ( head (parse (show f)))

ex2 :: IO ()
ex2 = do
        -- TODO: calculate size of a form with maxSize 10 and recursion level 5
        -- to show it is sufficient
        -- Can also do different types: low recursion, high size
        quickCheckWith stdArgs { maxSize = 15 } $ forAll form prop_ParseShow
