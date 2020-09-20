module Exercise1
where
    
import Lecture3

-- Time spent: 3h

-- Evaluates all valuations of a Form and retuns a list of results for each valuation (True or False)
getEvaluations :: Form -> [Bool]
getEvaluations f = map (`evl` f) (allVals f)

-- A Form is a contradiction if all the evaluations return False
contradiction :: Form -> Bool
contradiction f = all (\ x -> not(evl x f)) (allVals f)


-- A Form is a tautology if all the evaluations return True
tautology :: Form -> Bool
tautology f = all (\ x -> (evl x f)) (allVals f)


getValsThatSatisfy :: Form -> [Valuation]
getValsThatSatisfy f = filter (\x -> (evl x f)) (allVals f)

-- f2 logically entails f1 if and only if all the valudations that satisfy f2 also satisfy f1 
entails :: Form -> Form -> Bool
entails f1 f2 = all (\ x -> (evl x f1)) (getValsThatSatisfy f2)

-- Logical Equivalence
-- Two forms are equivalent if they have the same values for all inputs
equiv :: Form -> Form -> Bool
equiv f1 f2 = getEvaluations f1 == getEvaluations f2

main :: IO()
main = do
    -- Test data
    let form_1 = Dsj [p, q] -- 10, 01, 11
    let form_2 = Cnj [Neg(p), Neg(q)]
    let form_3 = Cnj [form_1, form_2]
    let form_4 = Dsj [p, Neg(p)]
    let form_5 = Dsj [Dsj[p, q], Cnj[Neg(p), Neg(q)]]
    let form_6 = Neg (p)
    let form_7 = Neg (q)

    putStrLn "Testing Contradiction"
    print (contradiction form_1) -- Failed test, form_1 is not a Contradiction
    print (contradiction form_3) -- Successful test, form_3 is a contradiction, has all the answers False

    putStrLn "Testing Tautology"
    print (tautology form_4) -- Successful test, fomr_4 is a Tautology
    print (tautology form_5) -- Successful test, fomr_5 is a Tautology
    print (tautology form_1) -- Failed test, form_1 is not a Tautology

    putStrLn "Testing Logical Entailment"
    print (entails form_1 form_2) -- Failed test, form_1  and form_2 are not Logical Entailed

    putStrLn "Testig Equivalence"
    print (equiv form_6 form_7) -- Succesful test, form_6 and form_7 are equivalent
    print (equiv form_2 form_3) -- Failed test, form_2  and form_3 are not equivalent
