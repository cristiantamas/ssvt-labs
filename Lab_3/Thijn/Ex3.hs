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

-- Ex 3
-- Time: start: 1.5 hours so far

applyDistLaw :: Form -> Form
applyDistLaw (Prop x) = Prop x -- base case: atom
applyDistLaw (Neg (Prop x)) = Neg (Prop x) -- base case: negated atom
applyDistLaw (Dsj [c,Cnj cs]) = Cnj $ map (\x -> Dsj [c, x]) cs -- changing to CNF equivalent form
applyDistLaw (Dsj [Cnj cs, c]) = Cnj $ map (\x -> Dsj [c, x]) cs -- changing to CNF equivalent form
applyDistLaw (Dsj (a:Cnj bs:cs)) = applyDistLaw $ Dsj (Cnj (map (\ x -> Dsj [a, x]) bs) : cs) -- changing to CNF equivalent form
applyDistLaw (Dsj ds) = Dsj (map applyDistLaw ds) -- traverse further
applyDistLaw (Cnj cs) = Cnj (map applyDistLaw cs) -- traverse further

-- applyDistLaw' :: Form -> Form
-- applyDistLaw' f | isCnf cnff = cnff
--                 | otherwise = applyDistLaw' cnff
--                 where cnff = applyDistLaw f

cnf :: Form -> Form
cnf f = applyDistLaw $ (nnf . arrowfree) f

isSimpleAtom :: Form -> Bool
isSimpleAtom (Prop x) = True
isSimpleAtom (Neg (Prop x)) = True
isSimpleAtom _ = False

isSimpleDsj :: Form -> Bool
isSimpleDsj (Dsj ds) = all isSimpleAtom ds
isSimpleDsj _ = False

isCnf :: Form -> Bool
isCnf (Dsj ds) = isSimpleDsj (Dsj ds)
isCnf (Cnj cs) = all isSimpleDsj cs
isCnf x = isSimpleAtom x

prop_cnf :: Form -> Bool
prop_cnf f = isCnf $ cnf f

ex3 :: IO ()
ex3 = do
        quickCheckWith stdArgs { maxSize = 10 } $ forAll form prop_cnf
        print $ prop_cnf (Dsj [p, Cnj [q, r]])