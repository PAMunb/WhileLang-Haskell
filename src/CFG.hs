module CFG where

import Syntax

import Prelude hiding (init)
import Data.Set 

init :: Stmt -> Label
init (Assignment _ _ l) = l
init (Skip l) = l
init (Seq s1 _) = init s1
init (IfThenElse (_, l) _ _) = l
init (While (_, l) _) = l 

final :: Stmt -> Set Label
final (Assignment _ _ l) = singleton l
final (Skip l) = singleton l
final (Seq s1 s2) = final s2
final (IfThenElse _ s1 s2) = final s1 `union` final s2  
final (While (_, l) _) =  singleton l 


blocks :: Stmt -> Set Blocks
blocks (Assignment a b c) = singleton (BlocksStmt (Assignment a b c)) 
blocks (Skip l) = singleton (BlocksStmt (Skip l))
blocks (Seq s1 s2) = blocks s1 `union` blocks s2  
blocks (IfThenElse testExp s1 s2) = (singleton (BlocksTest testExp)) `union` (blocks s1 `union` blocks s2)
blocks (While testExp s) =  (singleton (BlocksTest testExp)) `union` (blocks s)


