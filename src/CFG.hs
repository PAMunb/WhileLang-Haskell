module CFG where

import Syntax

import Prelude hiding (init)
import Data.Set 

-- TODO: Solve the undefined implementations

init :: Stmt -> Label
init (Assignment _ _ l) = l
init (Skip l) = l
init (Seq s1 _) = init s1
init (IfThenElse _ _ _) = undefined
init (While _ _) = undefined 

final :: Stmt -> Set Label
final (Assignment _ _ l) = singleton l
final (Skip l) = singleton l
final (Seq s1 s2) = final s2
final (IfThenElse _ s1 s2) = final s1 `union` final s2  
final (While _ _) = undefined 


