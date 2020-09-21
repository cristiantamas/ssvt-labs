import SetOrd
import Lecture3

type Name = Int

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


nsub :: Form -> Int
nsub (Prop x) = 1 
nsub (Neg f) = 1
nsub f@(Cnj [f1,f2]) = 1 +  (nsub f1) + (nsub f2)
nsub f@(Dsj [f1,f2]) = 1 + (nsub f1) + (nsub f2)
nsub f@(Impl f1 f2) = 1 + (nsub f1) +  (nsub f2)
nsub f@(Equiv f1 f2) = 1 +  (nsub f1) + (nsub f2)


main :: IO()
main = do
    let form_1 = Cnj [p, Dsj[q, r]]

    putStr "Initial formula: " 
    print (form_1)

    putStr "Sets of sub-formulae: "
    print (sub form_1)

    putStr "Number of sub-formulae: "
    print (nsub form_1)


-- TEST EXAMPLES --

-- 1.
--Initial formula: *(1 +(2 3))
--Sets of sub-formulae: {1,2,3,*(1 +(2 3)),+(2 3)}
--Number of sub-formulae: 5