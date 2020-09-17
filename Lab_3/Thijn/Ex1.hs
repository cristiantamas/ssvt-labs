-- Lab3 exercises
-- Thijn Albers

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import GHC.Base (liftM2, liftM)


-- Ex1
-- Time: ~2 hours

allEvls :: Form -> [Bool]
allEvls f = map (`evl` f) (allVals f)

-- A form is a contradiction if all of its evaluations return false
contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)

-- A form is a tautology if all of its evaluations return true
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- -- | logical entailment
-- P and Q are logically entailed if its evalations are of the form Q if P, otherwise True.
-- The form must have the same amount of evaluations.
entails :: Form -> Form -> Bool
entails f1 f2   | (length $ allVals f1) /= (length $ allVals f2) = False
                | otherwise =
                    all (\v -> if evl v f1 then evl v f1 --> evl v f2 == evl v f2 else evl v f1 --> evl v f2 ) (allVals f1)

-- -- | logical equivalence
-- P and Q are logically equivalent if they have the same truth values for all inputs
-- The form must have the same amount of evaluations.
equiv :: Form -> Form -> Bool
equiv f1 f2 = allEvls f1 == allEvls f2

-- Forms to test with
-- Forms are examples of equivlances / tautologies / contradictions / entailments
form4 = Neg (Cnj [p, q])
form5 = Dsj [Neg p, Neg q]
form6 = Dsj [p, Cnj [q, r]]
form7 = Cnj [Dsj [p, q], Dsj [p, r]]
form8 = Impl p q
form9 = Dsj [Neg p, q]
form10 = Dsj [p, Neg p]

ex1 :: IO ()
ex1 = do
    putStrLn "== Testing Equivalence =="
    print $ equiv form4 form5
    print $ equiv form4 form4
    print $ equiv form6 form7
    print $ equiv form8 form9

    putStrLn "== Testing False Equivalence =="
    print $ equiv form3 form5
    print $ equiv form4 form1
    print $ equiv form2 form8
    print $ equiv form8 form6

    putStrLn "== Testing True Tautology =="
    print $ tautology form1
    print $ tautology form10
    print $ equiv form6 form7
    print $ equiv form8 form9

    putStrLn "== Testing False Tautology =="
    print $ tautology form4
    print $ tautology form5
    print $ tautology form6
    print $ tautology form7
    print $ tautology form8
    print $ tautology form9