import Lecture3



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
covnert_to_cnf :: Form -> Form
covnert_to_cnf (Prop x) = Prop x
covnert_to_cnf (Neg (Prop x)) = Neg (Prop x)
covnert_to_cnf (Neg (Neg f)) = covnert_to_cnf f
convert_to_cnf (Cnj fs) = Cnj (map distributive_law fs)
convert_to_cnf (Dsj fs) = distributive_law (Dsj fs)


-- Distributive law for a Disjunction
-- A V (B ^ C) <--> (A V B) ^ (A V C)
-- If we react a literal, a negation of a literal, we return it
-- If we reach a Conjunction, we apply the distributive law over all the elements of the conjunction
-- If we react a Disjunction between an element and a Conjunction, we distribute that element to all the
-- elements of the Conjunction just like in the rule above
distributive_law :: Form -> Form
distributive_law (Prop x) = Prop x
distributive_law (Neg (Prop x)) = Neg (Prop x)
distributive_law (Dsj [f1, Cnj fs]) = Cnj ((map (\ f -> Dsj[f1, f])) (map distributive_law fs))
distributive_law (Dsj [Cnj fs, f1]) = Cnj ((map (\ f -> Dsj[f, f1])) (map distributive_law fs))

main :: IO()
main = do
    let form_1 = Impl p q
    let form_2 = Equiv r form_1

    putStrLn "Conversion to CNF"
    putStr "Initial formula: " 
    print (form_2)

    putStr "Arrowfree formula: " 
    print (arrowfree form_2)

    putStr "NNF formula: " 
    print (convert_to_nnf form_2)
    putStr "CNF conversion: "
    print (convert_to_cnf (convert_to_nnf form_2))