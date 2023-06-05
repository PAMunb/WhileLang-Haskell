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
 deriving(Eq, Ord) 

instance Show AExp where
    show (Var var) = show var
    show (Const int) = show int
    show (Add a1 a2) = show a1 <> "+" <> show a2
    show (Sub a1 a2) = show a1 <> "-" <> show a2
    show (Mult a1 a2) = show a1 <> "*" <> show a2

data BExp = CTrue             -- True constant
          | CFalse            -- False constant
          | Not BExp
          | And BExp BExp
          | Or  BExp BExp
          | EQExp AExp AExp
          | GTExp AExp AExp
          | LTExp AExp AExp
 deriving(Eq, Ord)  

instance Show BExp where
    show (CTrue) = "true"
    show (CFalse) = "false"
    show (Not b) = "¬" <> show b
    show (And b1 b2) = show b1 <> "&" <> show b2
    show (Or b1 b2) = show b1 <> "||" <> show b2
    show (EQExp a1 a2) = show a1 <> "==" <> show a2
    show (GTExp a1 a2) = show a1 <> ">" <> show a2
    show (LTExp a1 a2) = show a1 <> "<" <> show a2

data Stmt = Assignment Id AExp Label    -- Assignment
          | Skip Label
          | Seq Stmt Stmt
          | IfThenElse TestExp Stmt Stmt
          | While TestExp Stmt
 deriving(Eq, Ord)

instance Show Stmt where
    show (Assignment var a l) = "[" <> show var <> ":=" <> show a <> "] " <> show l
    show (Skip l) = "[skip] " <> show l
    show (Seq s1 s2) = show s1 <> "; " <> show s2
    show (IfThenElse test s1 s2) = "if " <> show test <> " then " <> show s1 <> " else " <> show s2
    show (While (test, l) s) = "while " <> "[" <> show test <> "] " <> show l <> " do " <> show s

data Blocks = BlocksStmt Stmt 
            | BlocksTest TestExp 
 deriving(Eq, Ord)

instance Show Blocks where
    show (BlocksStmt stmt) = show stmt
    show (BlocksTest (test, l)) = "[" <> show test <> "] " <> show l


