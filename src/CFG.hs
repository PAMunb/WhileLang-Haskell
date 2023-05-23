module CFG where

import Syntax

import Prelude hiding (init)
import Data.Set 
import Data.Foldable


init :: Stmt -> Label
init (Assignment _ _ l) = l
init (Skip l) = l
init (Seq s1 _) = init s1
init (IfThenElse (_, l) _ _) = l
init (While (_, l) _) = l

final :: Stmt -> Set Label
final (Assignment _ _ l) = singleton l
final (Skip l) = singleton l
final (Seq _ s2) = final s2
final (IfThenElse _ s1 s2) = final s1 `union` final s2
final (While (_, l) _) =  singleton l

blocks :: Stmt -> Set Blocks
blocks (Assignment a b c) = singleton (BlocksStmt (Assignment a b c))
blocks (Skip l) = singleton (BlocksStmt (Skip l))
blocks (Seq s1 s2) = blocks s1 `union` blocks s2
blocks (IfThenElse testExp s1 s2) = singleton (BlocksTest testExp) `union` (blocks s1 `union` blocks s2)
blocks (While testExp s) =  singleton (BlocksTest testExp) `union` blocks s

findBlock :: Label -> Program -> Maybe Blocks
findBlock label prog = findB (blocks prog)
    where
        findB :: Set Blocks -> Maybe Blocks
        findB blcks = Data.Foldable.find matchLabel blcks

        matchLabel :: Blocks -> Bool
        matchLabel (BlocksStmt stm) = do
            case stm of
                Assignment _ _ l ->  l == label
                Skip l ->  l == label
                _ -> False
        matchLabel (BlocksTest (_, l)) = l == label

labels :: Stmt -> Set Label
labels (Assignment _ _ l) = singleton l
labels (Skip l) = singleton l
labels (Seq s1 s2) = labels s1 `union` labels s2
labels (IfThenElse (_, l) s1 s2) = singleton l `union` labels s1 `union` labels s2
labels (While (_, l) s) =  singleton l `union` labels s

fv :: Stmt -> Set Id
fv (Assignment var a _) = singleton var `union` getVarFromAExp a
fv (Skip {}) = Data.Set.empty
fv (Seq s1 s2) = fv s1 `union` fv s2
fv (IfThenElse _ s1 s2) = fv s1 `union` fv s2
fv (While _ s) =  fv s

getVarFromAExp :: AExp -> Set Id
getVarFromAExp (Var var) = singleton var
getVarFromAExp (Const int) = empty
getVarFromAExp (Add a1 a2) = getVarFromAExp a1 `union` getVarFromAExp a2
getVarFromAExp (Sub a1 a2) = getVarFromAExp a1 `union` getVarFromAExp a2
getVarFromAExp (Mult a1 a2) = getVarFromAExp a1 `union` getVarFromAExp a2

getVarFromBExp :: BExp -> Set Id
getVarFromBExp (Not not) = getVarFromBExp not
getVarFromBExp (And b1 b2) = getVarFromBExp b1 `union` getVarFromBExp b2
getVarFromBExp (Or b1 b2) = getVarFromBExp b1 `union` getVarFromBExp b2
getVarFromBExp (EQExp b1 b2) = getVarFromAExp b1 `union` getVarFromAExp b2
getVarFromBExp (GTExp b1 b2) = getVarFromAExp b1 `union` getVarFromAExp b2
getVarFromBExp (LTExp b1 b2) = getVarFromAExp b1 `union` getVarFromAExp b2

getVars :: Program -> Set Id
getVars (Assignment _ aexp _)  = getVarFromAExp aexp
getVars (Skip _) = empty
getVars (Seq s1 s2)  = getVars s1 `union` getVars s2
getVars (IfThenElse (bExp, _) s1 s2)  = getVarFromBExp bExp `union` getVars s1 `union` getVars s2
getVars (While (bExp, _) s)  = getVarFromBExp bExp `union` getVars s

assigments :: Stmt -> Set (Id, Label)
assigments (Assignment var _ l) = singleton (var, l)
assigments (Skip {}) = Data.Set.empty
assigments (Seq s1 s2) = assigments s1 `union` assigments s2
assigments (IfThenElse _ s1 s2) = assigments s1 `union` assigments s2
assigments (While _ s) =  assigments s

flow :: Stmt -> Set (Label, Label)
flow (Assignment {}) = empty
flow (Skip {}) = empty
flow (Seq s1 s2) = do
    let s2Init = init s2
    let s1Final = final s1
    let setOfLabels = Data.Set.map (\s1Label -> (s1Label, s2Init)) s1Final
    flow s1 `union` flow s2  `union` setOfLabels
flow (IfThenElse (_, l) s1 s2) = flow s1 `union` flow s2 `union` singleton (l, init s1)  `union` singleton (l, init s2)
flow (While (_, l) s) = do
    let sFinal = final s
    let setOfLabels = Data.Set.map (\sLabel -> (sLabel, l)) sFinal
    flow s `union`  singleton (l, init s)  `union` setOfLabels

labelsV2 :: Stmt -> Set Label
labelsV2 s = do
    let flowS = flow s
    singleton (init s) `union` Data.Set.map fst flowS `union` Data.Set.map snd flowS

flowR :: Stmt -> Set (Label, Label)
flowR s = do
    let flowS = flow s
    Data.Set.map (\(l, l') -> (l', l)) flowS
