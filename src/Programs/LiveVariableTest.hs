module Programs.LiveVariableTest where 

import Syntax

s01, s02, s03, s04, thenS1, thenS2, s05 :: Stmt
ifTeste :: (BExp, Label)

s01 = Assignment "x" (Const 2) 1
s02 = Assignment "y" (Const 4) 2
s03 = Assignment "x" (Const 1) 3

ifTeste = (GTExp (Var "y") (Var "x"), 4)
thenS1 = Assignment "z" (Var "y") 5
thenS2 = Assignment "z" (Mult (Var "y") (Var "y")) 6
s04 = IfThenElse ifTeste thenS1 thenS2

s05 = Assignment "x" (Var "z") 7

lvtest :: Program
lvtest = Seq s01 (Seq s02 (Seq s03 (Seq s04 s05)))