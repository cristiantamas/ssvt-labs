{- File: Ex6.hs
 - Author: Leon Kielstra
 - Lab 3 Exercise 6
-}

import Data.List (nub)
import Ex1 ( equiv )
import Lecture3
import Test.QuickCheck
import Generators

type Clause = [Int]

type Clauses = [Clause]

{- We can assume that form is in CNF. We need the following elements to convert it to clauses:
 -   > Map the conjuncts to Clauses
 -   > Map the disjuncts to a Clause
 -   > Map the literals to an Int
-}

{- We assume that the given form is in cnf, therefore when we do not encounter a conjunction
 - we can assume that it is a single literal.
-}
cnf2cls :: Form -> Clauses
cnf2cls (Cnj disjuncts) = map disjunctToClause disjuncts
cnf2cls cnf = [disjunctToClause cnf]

{- We only pass disjuncts or literals to this function. Therefore if it is not a disjunct
 - we can assume it is a literal and map it to an integer.
-}
disjunctToClause :: Form -> Clause
disjunctToClause (Dsj literals) = map literalToInt literals
disjunctToClause literal = [literalToInt literal]

{- Here we map the literal to an integer. In case the higher level functions don't pass a literal,
 - it raises an error here.
-}
literalToInt :: Form -> Int
literalToInt (Prop p) = p
literalToInt (Neg (Prop p)) = - p
literalToInt _ = error "Form is not a literal"

--- Testing ---
-- Test examples
cnf_1 = Cnj [(Dsj [p, q]), (Dsj [Neg p, q]), (Dsj [q, Neg q])]

cnf_2 = (Dsj [p, q, r])

cnf_3 = Neg p

cnf_4 = p

-- Helper functions for testing
countConjunctions :: Form -> Int
countConjunctions (Cnj conjunctions) = length conjunctions
countConjunctions cnf = 1

countAtoms :: Clauses -> Int
countAtoms clauses = length (nub (map (\atom -> abs atom) (concat clauses)))

{- Here we define the functions to convert clauses to DNF. It basically needs the same steps as above
 -   > Map clauses list to conjuncts of clauses
 -   > Map clause list to disjuncts of integers
 -   > Map integers to Props
-}
clausesToCnf :: Clauses -> Form
clausesToCnf clauses = Cnj (map clauseToDnf clauses)

clauseToDnf :: Clause -> Form
clauseToDnf clause = Dsj (map literalToProp clause)

literalToProp :: Int -> Form
literalToProp literal
  | literal < 0 = Neg (Prop (abs literal))
  | otherwise = Prop literal

ex6 :: IO ()
ex6 = do
  -- Property 1: Length of the list of clauses if the same as length of the list of the conjunctions
  putStrLn "== Conjunctions length =="
  print $ countConjunctions cnf_1 == length (cnf2cls cnf_1) -- True
  print $ countConjunctions cnf_2 == length (cnf2cls cnf_2) -- True
  print $ countConjunctions cnf_3 == length (cnf2cls cnf_3) -- True
  print $ countConjunctions cnf_4 == length (cnf2cls cnf_4) -- True

  -- Property 2: Number of Props in the form is the same size as the number of atoms in the clauses
  putStrLn "== Length Atoms equals length of Props =="
  print $ countAtoms (cnf2cls cnf_1) == length (propNames cnf_1) -- True
  print $ countAtoms (cnf2cls cnf_2) == length (propNames cnf_2) -- True
  print $ countAtoms (cnf2cls cnf_3) == length (propNames cnf_3) -- True
  print $ countAtoms (cnf2cls cnf_4) == length (propNames cnf_4) -- True

  {- We can also write a function that converts a clause to a cnf. Than we can use "equiv :: Form -> Form -> Bool"
   - from Ex1 to check if the resulting forms are equivalent. The downside of this way of testing is that it depends
   - on your confidence in the clausesToCnf function.
  -}
  putStrLn "== Clauses to CNF with example data =="
  print $ equiv (clausesToCnf (cnf2cls cnf_1)) cnf_1 -- True
  print $ equiv (clausesToCnf (cnf2cls cnf_2)) cnf_2 -- True
  print $ equiv (clausesToCnf (cnf2cls cnf_3)) cnf_3 -- True
  print $ equiv (clausesToCnf (cnf2cls cnf_4)) cnf_4 -- True
  -- We can also use quickcheck to generate test cases for this
  quickCheck $ forAll cnfform (\form -> equiv form (clausesToCnf (cnf2cls form))) -- +++ OK, passed 100 tests.
