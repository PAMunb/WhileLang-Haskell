module Syntax where 

type Id = String 
type Label = Integer
type Program = Stmt
type TestExp = (BExp, Label)

data AExp = Var Id            -- variables
          | Const Integer     -- constants
          | Add AExp AExp     
          | Sub AExp AExp
          | Mult AExp AExp 
 deriving(Eq, Show, Ord) 

data BExp = CTrue             -- True constant
          | CFalse            -- False constant
          | Not BExp
          | And BExp BExp
          | Or  BExp BExp
          | EQExp AExp AExp
          | GTExp AExp AExp
          | LTExp AExp AExp
 deriving(Eq, Show, Ord)  

data Stmt = Assignment Id AExp Label    -- Assignment
          | Skip Label
          | Seq Stmt Stmt
          | IfThenElse TestExp Stmt Stmt
          | While TestExp Stmt
 deriving(Eq, Show, Ord)


data Blocks = BlocksStmt Stmt | BlocksTest TestExp deriving(Eq, Show, Ord)