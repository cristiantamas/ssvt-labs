-- Lab3 exercises
-- Thijn Albers
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

form' :: Integral a => a -> Gen Form
form' 0 = fmap Prop name2
form' n | n > 0 = oneof [fmap Prop name2,
              fmap Neg subform,
              Cnj <$> listOf subform,
              Dsj <$> listOf subform,
              liftM2 Impl subform subform,
              liftM2 Equiv subform subform]
              where subform = form' (n `div` 2)

applyDistLaw :: Form -> Form
applyDistLaw (Prop x) = Prop x -- base case: atom
applyDistLaw (Neg (Prop x)) = Neg (Prop x) -- base case: negated atom
applyDistLaw (Dsj [c,Cnj cs]) = Cnj $ map (\x -> Dsj [c, x]) cs -- changing to CNF equivalent form
applyDistLaw (Dsj [Cnj cs, c]) = Cnj $ map (\x -> Dsj [c, x]) cs -- changing to CNF equivalent form
applyDistLaw (Dsj (a:Cnj bs:cs)) = applyDistLaw $ Dsj (Cnj (map (\ x -> Dsj [a, x]) bs) : cs) -- changing to CNF equivalent form
applyDistLaw (Dsj ds) = Dsj (map applyDistLaw ds) -- traverse further
applyDistLaw (Cnj cs) = Cnj (map applyDistLaw cs) -- traverse further

cnf :: Form -> Form
cnf f = applyDistLaw $ (nnf . arrowfree) f

-- Ex 4
-- Time start: 2.5 hours

-- Generator for logical expressions in cnf form
cnfform :: Gen Form
cnfform = Cnj <$> listOf simpleDsj

simpleatom :: Gen Form
simpleatom = oneof [fmap Prop name2, fmap (Neg . Prop) name2]

simpleDsj :: Gen Form
simpleDsj = oneof [simpleatom, Dsj <$> listOf simpleatom]

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
prop_cnf :: Form -> Bool
prop_cnf f = isCnf $ cnf f

-- The cnf conversion depends on arrowfree and nnf functions
-- so these should be tested too

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

-- I would have liked to calculate the amount of redundant testcases where
-- the precondition is not satisfied, but I have not managed to do so
-- The chance that a randomly generated is already arrowfree is existent, but not
-- so large that I consider this hoare test useless
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
        -- I could not use the generator as I would add the condition that isCnf is false
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