module Programs.AvailableExpressionsExample where 

import Syntax

s01, s02, s03, whileS1, whileS2 :: Stmt
whileTeste :: (BExp, Label)

s01 = Assignment "x" (Add (Var "a") (Var "b")) 1
s02 = Assignment "y" (Mult (Var "a") (Var "b")) 2

whileTeste = (GTExp (Var "y") (Add (Var "a") (Var "b")), 3)
whileS1 = Assignment "a" (Add (Var "a") (Const 1)) 4
whileS2 = Assignment "x" (Add (Var "a") (Var "b")) 5
s03 = While whileTeste (Seq whileS1 whileS2)

aeExample :: Program
aeExample = Seq s01 (Seq s02 s03)
