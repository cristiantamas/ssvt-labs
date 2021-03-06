module Generators where

import Data.List
import GHC.Base (liftM, liftM2)
import Lecture3
import System.Random
import Test.QuickCheck

form :: Gen Form
form = sized form'

instance Arbitrary Form where
  arbitrary = form

name :: Gen Name
name = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

name2 :: Gen Name
name2 = choose (1, 10)

-- Heavily inspired by "Generating Recursive Data Types" from http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html
-- The generation of forms is recursive and the size of a form generated can quickly grow
-- This makes the generation very computationally intensive
-- To counter this we create an upper bound based on the given size of the tree,
-- so that we may manipulate the size for different scenarios

-- One flaw of the generator is that it can generate disjunctions and conjunctions
-- of length 1
form' :: Integral a => a -> Gen Form
form' 0 = fmap Prop name2
form' n
  | n > 0 =
    oneof
      [ fmap Prop name2,
        fmap Neg subform,
        Cnj <$> listOf1 subform,
        Dsj <$> listOf1 subform,
        liftM2 Impl subform subform,
        liftM2 Equiv subform subform
      ]
  where
    subform = form' (n `div` 2)

-- Generator for logical expressions in cnf form
cnfform :: Gen Form
cnfform = Cnj <$> listOf1 simpleDsj

simpleatom :: Gen Form
simpleatom = oneof [fmap Prop name2, fmap (Neg . Prop) name2]

simpleDsj :: Gen Form
simpleDsj = oneof [simpleatom, Dsj <$> listOf simpleatom]
