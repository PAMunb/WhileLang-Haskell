module PowerProgram where 

import Syntax

s01, s02, whileS1, whileS2 :: Stmt
whileTeste :: (BExp, Label)

s01 = Assignment "z" (Const 1) 1

whileTeste = (GTExp (Var "x") (Const 0), 2)
whileS1 = Assignment "z" (Mult (Var "z") (Var "y")) 3
whileS2 = Assignment "x" (Sub (Var "x") (Const 1)) 4
s02 = While whileTeste (Seq whileS1 whileS2)


power :: Program
power = Seq s01 s02

