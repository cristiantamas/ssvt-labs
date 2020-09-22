import Lecture3
import Data.List (nub)
import Data.String


{- A logical contradiction can be checked by:
 - For all possible combinations of values for each property (allVals in Lecture3) of a form,
 - it always evaluates (evl in Lecture 3) to false.
-}
contradiction :: Form -> Bool
contradiction form = all (\value -> not (evl value form)) (allVals form)

{- A logical tautology can be checked by:
 - For all possible combinations of values for each property (allVals in Lecture3) of a form,
 - it always evaluates (evl in Lecture 3) to true.
-}
tautology :: Form -> Bool
tautology form = all (\value -> evl value form) (allVals form)

{- Logical entailment means that form 2 always evaluates to True when form 1 evaluates to True
 - To check this we can check whether form 1 ==> form 2 evaluates to True for all possible combinations
 - of values for each property. (a.k.a whether that's a tautology)
-}
entails :: Form -> Form -> Bool
entails form_1 form_2 = tautology (Impl form_1 form_2)

{- Logical equivalence means that for all possible combinations of values for each property, form 1 <=> form 2
 - always evaluates to True. Again we can solve this with checking for a tautology.
-}
equiv :: Form -> Form -> Bool
equiv form_1 form_2 = tautology (Equiv form_1 form_2)

--- Testing ---
-- OUTPUT
-- == Contradiction Examples ==
-- True
-- True
-- == Tautology Examples ==
-- True
-- True
-- == Entails Example ==
-- True
-- == Equivalence Examples ==
-- True
-- True
-- True
-- == allVals tests ==
-- True
-- True
-- True
-- == Form tests
-- True
-- True
ex1 :: IO ()
ex1 = do
{- Testing these definitions can be done by testing it with forms of which we know for sure that they
 - satisfy the definition. This is only high level testing and only shows us whether it works for those examples and
 - not necessarily whether it also returns the correct answer for forms that do not satisfy the definition.
-}
  putStrLn "== Contradiction Examples =="
  print $ contradiction (Neg form1) -- True
  print $ contradiction (Neg form3) -- True

  putStrLn "== Tautology Examples =="
  print $ tautology form1 -- True
  print $ tautology form3 -- True

  putStrLn "== Entails Example =="
  print $ entails form2 form3 -- True

  putStrLn "== Equivalence Examples =="
  -- Some examples from https://en.wikipedia.org/wiki/Logical_equivalence
  print $ equiv (Impl p q) (Dsj [(Neg p), q]) -- True
  print $ equiv (Impl p q) (Impl (Neg q) (Neg p)) -- True
  print $ equiv (Cnj [p, q]) (Neg (Impl p (Neg q))) -- True

{- We can also test smaller functions used in the definitions.
 - For example whether allVals returns all possible combinations.
 - Or whether Impl and Equiv return the correct forms.
-}
  putStrLn "== allVals tests =="
  -- We check if the number of unique items returned by allVals are the same as 2^(number of properties).
  print $ length (allVals form1) == length (nub (allVals form1)) -- True
  print $ length (nub (allVals form1)) == 4 -- True
  print $ length (nub (allVals form3)) == 8 -- True

  putStrLn "== Form tests"
  print $ show (Impl form1 form2) == "(" ++ show form1 ++ "==>" ++ show form2 ++ ")" -- True
  print $ show (Equiv form1 form2) == "(" ++ show form1 ++ "<=>" ++ show form2 ++ ")" -- True
