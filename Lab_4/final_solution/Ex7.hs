-- Lab 4
-- Exercise 7

import Ex3 (symClos)
import Ex5 (trClos)
import Ex6 (genRandomRelation)
import Test.QuickCheck ( forAll, quickCheck )

type Rel a = [(a, a)]

{- We can implement the lab question as a property and test it with QuickCheck, see if that results in
- any counter examples. If it does not this isn't necessarily proof that the property is correct, since
- the generated data might not cover all testcases.
-}

prop_ex7 :: (Eq a, Ord a) => Rel a -> Bool
prop_ex7 relation = trClos (symClos relation) == symClos (trClos relation)

ex7 :: IO ()
ex7 = do
  quickCheck $ forAll genRandomRelation prop_ex7

{- Some examples of outputs by this:
 -
 - ```
 - *** Failed! Falsified (after 3 tests):
 - [(2,9)]
 - *Main Data.List> ex7
 - *** Failed! Falsified (after 3 tests):
 - [(7,10),(4,7)]
 - *Main Data.List> ex7
 - *** Failed! Falsified (after 2 tests):
 - [(8,1)]
 - ```
 -
 - From this we can conclude that these are not the same. Proven by contradiction.
-}