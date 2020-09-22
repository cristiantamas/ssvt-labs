-- Lab3 exercises
-- Exercise 4
-- Time indication: 3 hours

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import GHC.Base (liftM2, liftM)
import Generators

-- From Ex3
convertToNnf :: Form -> Form
convertToNnf f = nnf (arrowfree f)

convertToCnf :: Form -> Form
convertToCnf (Prop x) = Prop x
convertToCnf (Neg (Prop x)) = Neg (Prop x)
convertToCnf (Neg (Neg f)) = convertToCnf f
convertToCnf (Dsj[a, b])
    | isSimpleAtom a && isSimpleAtom b = Dsj[a, b]
convertToCnf (Cnj[a, b])
    | isSimpleAtom a && isSimpleAtom b = Cnj[a, b]
convertToCnf (Cnj[f1, f2]) = Cnj[convertToCnf f1, convertToCnf f2]
convertToCnf (Dsj [f1, Dsj fs]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f1, f]) . convertToCnf) fs))
convertToCnf (Dsj [f1, Cnj fs]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f1, f]) . convertToCnf) fs))
convertToCnf (Dsj [Cnj fs, f1]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f, f1]) . convertToCnf) fs))
convertToCnf (Dsj [Dsj fs, f1]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f, f1]) . convertToCnf) fs))
convertToCnf (Dsj [Prop x, f2]) = Dsj [Prop x, f2]

cnf :: Form -> Form
cnf f = convertToCnf $ (nnf . arrowfree) f

-- Start Ex 4

-- Expressing the grammar of a CNF form
isSimpleAtom :: Form -> Bool
isSimpleAtom (Prop x) = True
isSimpleAtom (Neg (Prop x)) = True
isSimpleAtom _ = False

isSimpleDsj :: Form -> Bool
isSimpleDsj (Dsj ds) = all isSimpleAtom ds
isSimpleDsj x = isSimpleAtom x

isCnf :: Form -> Bool
isCnf (Dsj ds) = isSimpleDsj (Dsj ds)
isCnf (Cnj cs) = all isSimpleDsj cs
isCnf x = isSimpleDsj x

-- To ensure the validity of this property, we have to test isCnf as well
-- The cnf conversion depends on arrowfree and nnf functions
-- so these should be tested too
prop_cnf :: Form -> Bool
prop_cnf f = isCnf $ cnf f -- test the conversion
prop_cnf2 f = isArrowfree cnff && isNegationNormalForm cnff where cnff = cnf f -- forms in cnf should be arrowfree and in nnf

-- Test properties through a hoare triple
hoareTestForm :: (Form -> Bool) -> (Form -> Form) -> (Form -> Bool) -> Form -> Bool
hoareTestForm pre fx post x = pre x --> post (fx x)

-- This function could be tested for complete coverage
isArrowfree :: Form -> Bool
isArrowfree (Prop x) = True
isArrowfree (Neg x) = isArrowfree x
isArrowfree (Cnj xs) = all isArrowfree xs
isArrowfree (Dsj xs) = all isArrowfree xs
isArrowfree _ = False

test_arrowfree :: Form -> Bool
test_arrowfree = hoareTestForm (const True) arrowfree isArrowfree

-- This function could be tested for complete coverage
isNegationNormalForm :: Form -> Bool
isNegationNormalForm (Prop x) = True
isNegationNormalForm (Neg (Prop x)) = True
isNegationNormalForm (Neg x) = False -- negation on something other than a property
isNegationNormalForm (Cnj xs) = all isNegationNormalForm xs
isNegationNormalForm (Dsj xs) = all isNegationNormalForm xs
isNegationNormalForm _ = False

-- We would have liked to calculate the amount of redundant testcases where
-- the precondition is not satisfied, but we did not manage.
-- The chance that a randomly generated is already arrowfree is existent, but not
-- so large that we consider this hoare test insignificant.
test_negationNormalForm :: Form -> Bool
test_negationNormalForm = hoareTestForm isArrowfree nnf isNegationNormalForm

-- if the conversion only allows nnf forms as input this hoare triple
-- with nnf as precondition can be used
test_cnf :: Form -> Bool
test_cnf = hoareTestForm isNegationNormalForm cnf isCnf

ex4 :: IO ()
ex4 = do
        -- Tests for isCnf:
        -- These are all in CNF plus some tests with the generator
        -- I left the manual tests because they helped me find errors in the generator
        -- and isCnf function
        print $ isCnf (Cnj [p, Dsj [q, r]])
        print $ isCnf (Cnj [Dsj [p, Neg q, Neg r], Dsj [Neg p, q, r]])
        print $ isCnf (Cnj [Dsj [p, q], r])
        print $ isCnf (Cnj [p, r])
        print $ isCnf (Cnj [Neg p, Dsj [q, r, Neg p], Dsj [p, Neg r]])
        quickCheckWith stdArgs { maxSize = 10 } $ forAll cnfform isCnf

        -- These are not in CNF
        -- We could not use the generator as we would add the condition that isCnf is false
        -- However, we are testing isCnf so this would be a useless test
        -- You could write a generator which generates non Cnf forms, but making sure this generator
        -- generates "everything except CNFs", and not just a subset of "everything except CNFs" seems difficult.
        print $ isCnf (Cnj [p, q, r, Neg (Dsj [Neg p, Neg q])])
        print $ isCnf (Neg (Impl q (Dsj [Equiv p (Neg q), p])))
        print $ isCnf (Dsj [Cnj [p, q, r]])
        print $ isCnf (Equiv (Cnj [p, Dsj [q, r]]) (Cnj [p, r]))
        print $ isCnf (Cnj [Dsj [p, q, Neg r], Dsj [Neg r, p], Cnj [p, q, r]])

        -- Test for arrowfree
        quickCheckWith stdArgs { maxSize = 10 } $ forAll form test_arrowfree

        -- Test for negation normal form
        quickCheckWith stdArgs { maxSize = 10 } $ forAll form test_negationNormalForm

        -- High level test for the conversion of any form to cnf
        -- By testing the components of the conversion before, we can assume they work
        -- and any mistakes found are in our code
        quickCheckWith stdArgs { maxSize = 10 } $ forAll form prop_cnf
        quickCheckWith stdArgs { maxSize = 10 } $ forAll form prop_cnf2