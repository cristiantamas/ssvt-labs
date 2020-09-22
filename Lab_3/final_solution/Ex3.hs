{- File: Ex3.hs
 - Authors: Leon Kielstra, Christian Tamas, Joel Ruhe, Thijn Albers
 - Lab 3 Exercise 3
-}
-- Time spent: 5h

import System.Random
import Test.QuickCheck
import Lecture3
import GHC.Base (liftM2, liftM)

isSimpleAtom :: Form -> Bool
isSimpleAtom (Prop x) = True
isSimpleAtom (Neg (Prop x)) = True
isSimpleAtom _ = False



 -- The implementation uses the arrowfree and nnf functions declared in Lecture 3
 -- arrowfree: Takes a form and removes p-->q and p<-->q by replacing them with
 --             their equivalents: -p v q and (p^q)^(p v -q)
 -- After removing all the arrows we got a form that can be converted to a negation normal form
 -- nnf: Takes the form removes --p, -(p ^ q) and -(p v q) with their equivalents:
 --       p, (-p v -q) and (-p ^ -q)
convertToNnf :: Form -> Form
convertToNnf f = nnf (arrowfree f)


-- We apply the CNF over the form. If we react a Conjunction, we go over each sub-form of the conjunction
-- If we react a Disjunction, we apply the distributive law to it
--
-- Distributive law for a Disjunction
-- A V (B ^ C) <--> (A V B) ^ (A V C)
-- If we react a literal, a negation of a literal, we return it
-- If we reach a Conjunction, we apply the distributive law over all the elements of the conjunction
-- If we react a Disjunction between an element and a Conjunction, we distribute that element to all the
-- elements of the Conjunction just like in the rule above
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