{- File: Ex8.hs
 - Author: Leon Kielstra
 - Lab 1 Exercise 8
-}

import Lab1

accuses :: Boy -> Boy -> Bool
accuses a b
  | a == Matthew = not (b == Carl) && not (b == Matthew)
  | a == Peter = b == Matthew || b == Jack
  | a == Jack = not (accuses Matthew b) && not (accuses Peter b)
  | a == Arnold = (accuses Matthew b) /= (accuses Peter b)
  | a == Carl = not (accuses Arnold b)

accusers :: Boy -> [Boy]
accusers b = [a | a <- boys, accuses a b]

-- There are three boys who speak the truth. Therefore the boy who is accused by 3 other boys is guilty.
guilty :: [Boy]
guilty = [b | b <- boys, length (accusers b) == 3]

-- The three boys that accused the guilty boy are honest.
honest :: [Boy]
honest = [b | b <- boys, g <- guilty, accuses b g]
