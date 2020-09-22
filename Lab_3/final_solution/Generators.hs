module Generators
where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import GHC.Base (liftM2, liftM)

form :: Gen Form
form = sized form'

instance Arbitrary Form where
    arbitrary = form

name :: Gen Name
name = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

name2 :: Gen Name
name2 = choose (0,10)

-- Heavily inspired by "Generating Recursive Data Types" from http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html
-- The generation of forms is recursive and the size of a form generated can quickly grow
-- This makes the generation very computationally intensive
-- To counter this we set a maximum recursion level of 5, which still results in large forms
-- but computationally doable.
form' :: Integral a => a -> Gen Form
form' 0 = fmap Prop name2
form' n | n > 0 = oneof [fmap Prop name2,
              fmap Neg subform,
              Cnj <$> listOf1 subform,
              Dsj <$> listOf1 subform,
              liftM2 Impl subform subform,
              liftM2 Equiv subform subform]
              where subform = form' (n `div` 2)

-- Generator for logical expressions in cnf form
cnfform :: Gen Form
cnfform = Cnj <$> listOf1 simpleDsj

simpleatom :: Gen Form
simpleatom = oneof [fmap Prop name2, fmap (Neg . Prop) name2]

simpleDsj :: Gen Form
simpleDsj = oneof [simpleatom, Dsj <$> listOf simpleatom]