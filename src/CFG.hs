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
final (Seq _ s2) = final s2
final (IfThenElse _ s1 s2) = final s1 `union` final s2
final (While (_, l) _) =  singleton l

blocks :: Stmt -> Set Blocks
blocks (Assignment a b c) = singleton (BlocksStmt (Assignment a b c))
blocks (Skip l) = singleton (BlocksStmt (Skip l))
blocks (Seq s1 s2) = blocks s1 `union` blocks s2
blocks (IfThenElse testExp s1 s2) = singleton (BlocksTest testExp) `union` (blocks s1 `union` blocks s2)
blocks (While testExp s) =  singleton (BlocksTest testExp) `union` blocks s

getLabelFromBlock :: Blocks  -> Label
getLabelFromBlock (BlocksStmt stm) = do
    case stm of
        Assignment _ _ l -> l
        Skip l -> l
        Seq _ _ -> undefined
        IfThenElse (_, l) _ _ -> l
        While (_, l) _ -> l
getLabelFromBlock (BlocksTest (_, l)) = l

labels :: Stmt -> Set Label
labels s = Data.Set.map getLabelFromBlock (blocks s)

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