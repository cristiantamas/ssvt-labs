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

--Time spent: 5h

 -- The implementation uses the arrowfree and nnf functions declared in Lecture 3
 -- arrowfree: Takes a form and removes p-->q and p<-->q by replacing them with
 --             their equivalents: -p v q and (p^q)^(p v -q)
 -- After removing all the arrows we got a form that can be converted to a negation normal form
 -- nnf: Takes the form removes --p, -(p ^ q) and -(p v q) with their equivalents:
 --       p, (-p v -q) and (-p ^ -q)
convert_to_nnf :: Form -> Form
convert_to_nnf f = nnf (arrowfree f)


-- We apply the CNF over the form. If we react a Conjunction, we go over each sub-form of the conjunction
-- If we react a Disjunction, we apply the distributive law to it
--
-- Distributive law for a Disjunction
-- A V (B ^ C) <--> (A V B) ^ (A V C)
-- If we react a literal, a negation of a literal, we return it
-- If we reach a Conjunction, we apply the distributive law over all the elements of the conjunction
-- If we react a Disjunction between an element and a Conjunction, we distribute that element to all the
-- elements of the Conjunction just like in the rule above
convert_to_cnf :: Form -> Form
convert_to_cnf (Prop x) = Prop x
convert_to_cnf (Neg (Prop x)) = Neg (Prop x)
convert_to_cnf (Neg (Neg f)) = convert_to_cnf f
convert_to_cnf (Dsj[a, b]) 
    | isSimpleAtom a && isSimpleAtom b = Dsj[a, b]
convert_to_cnf (Cnj[a, b])
    | isSimpleAtom a && isSimpleAtom b = Cnj[a, b]
convert_to_cnf (Cnj[f1, f2]) = Cnj[convert_to_cnf f1, convert_to_cnf f2]
convert_to_cnf (Dsj [f1, Dsj fs]) =  convert_to_cnf (Cnj ((map (\ f -> Dsj[f1, f])) (map convert_to_cnf fs)))
convert_to_cnf (Dsj [f1, Cnj fs]) =  convert_to_cnf (Cnj ((map (\ f -> Dsj[f1, f])) (map convert_to_cnf fs)))
convert_to_cnf (Dsj [Cnj fs, f1]) =  convert_to_cnf (Cnj ((map (\ f -> Dsj[f, f1])) (map convert_to_cnf fs)))
convert_to_cnf (Dsj [Dsj fs, f1]) =  convert_to_cnf (Cnj ((map (\ f -> Dsj[f, f1])) (map convert_to_cnf fs)))
convert_to_cnf (Dsj [Prop x, f2]) = (Dsj [Prop x, f2])


cnf :: Form -> Form
cnf f = convert_to_cnf $ (nnf . arrowfree) f

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
prop_cnf f =  isCnf $ cnf f


main :: IO()
main = do
    let form_1 = Impl p q
    let form_2 = Equiv form_1 q

    let form_test = Equiv (Neg p) q

    putStrLn "Conversion to CNF"
    putStr "Initial formula: " 
    print (form_test)

    putStr "Arrowfree formula: " 
    print (arrowfree form_test)

    putStr "NNF formula: " 
    print (convert_to_nnf form_test)
    putStr "CNF conversion: "
    print (convert_to_cnf (convert_to_nnf form_test))


    quickCheckWith stdArgs { maxSize = 10 } $ forAll form prop_cnf



-- TEST EXAMPLES --

-- 1.
--Conversion to CNF
--Initial formula: (1<=>2)
--Arrowfree formula: +(*(1 2) *(-1 -2))
--NNF formula: +(*(1 2) *(-1 -2))
--CNF conversion: *(+(*(1 2) -1) +(*(1 2) -2))


-- 2
--Conversion to CNF
--Initial formula: ((1==>2)<=>2)
--Arrowfree formula: +(*(+(-1 2) 2) *(-+(-1 2) -2))
--NNF formula: +(*(+(-1 2) 2) *(*(1 -2) -2))
--CNF conversion: *(+(*(+(-1 2) 2) *(1 -2)) +(*(+(-1 2) 2) -2))
