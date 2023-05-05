module FactorialProgram where 

import Syntax

s01, s02, s03, s04 :: Stmt

s01 = Assignment "y" (Var "x") 1
s02 = Assignment "z" (Const 1) 2
whileTeste = (GTExp (Var "y") (Const 1), 3)
whileS1 = Assignment "x" (Mult (Var "z") (Var "y")) 4
whileS2 = Assignment "y" (Sub (Var "y") (Const 1)) 5
s03 = While whileTeste (Seq whileS1 whileS2)
s04 = Assignment "y" (Const 0) 6


factorial :: Program
factorial = Seq s01 (Seq s02 (Seq s03 s04))

