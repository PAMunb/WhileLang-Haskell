module CFG where

import Syntax

import Prelude hiding (init) 

-- TODO: Solve the undefined implementations

init :: Stmt -> Label
init (Assignment _ _ l) = l
init (Skip l) = l
init (Seq s1 _) = init s1
init (IfThenElse _ _ _) = undefined
init (While _ _) = undefined 

