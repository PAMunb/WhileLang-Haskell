module Programs.VeryBusyExpressionsExample where 

import Syntax

s01, s02, s03, s04, s05, thenS1, thenS2 :: Stmt
ifTeste :: (BExp, Label)

s02 = Assignment "x" (Sub (Var "b") (Var "a")) 2
s03 = Assignment "y" (Sub (Var "a") (Var "b")) 3

s04 = Assignment "y" (Sub (Var "b") (Var "a")) 4
s05 = Assignment "x" (Sub (Var "a") (Var "b")) 5

ifTeste = (GTExp (Var "a") (Var "b"), 1)
thenS1 = Seq s02 s03
thenS2 = Seq s04 s05
s01 =  IfThenElse ifTeste thenS1 thenS2

vbExample :: Program
vbExample = s01
