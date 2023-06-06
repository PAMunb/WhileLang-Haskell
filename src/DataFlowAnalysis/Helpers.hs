module DataFlowAnalysis.Helpers where

import Syntax

import Data.Set
import Data.List (foldl1')

fv :: Stmt -> Set Id
fv (Assignment var a _) = singleton var `union` getVarFromAExp a
fv (Skip {}) = empty
fv (Seq s1 s2) = fv s1 `union` fv s2
fv (IfThenElse _ s1 s2) = fv s1 `union` fv s2
fv (While _ s) =  fv s

fvAExp :: AExp -> Set Id
fvAExp (Var var) = singleton var
fvAExp (Const _) = empty
fvAExp (Add a1 a2) = fvAExp a1 `union` fvAExp a2
fvAExp (Sub a1 a2) = fvAExp a1 `union` fvAExp a2
fvAExp (Mult a1 a2) = fvAExp a1 `union` fvAExp a2

getVarFromAExp :: AExp -> Set Id
getVarFromAExp (Var var) = singleton var
getVarFromAExp (Const _) = empty
getVarFromAExp (Add a1 a2) = getVarFromAExp a1 `union` getVarFromAExp a2
getVarFromAExp (Sub a1 a2) = getVarFromAExp a1 `union` getVarFromAExp a2
getVarFromAExp (Mult a1 a2) = getVarFromAExp a1 `union` getVarFromAExp a2

getVarFromBExp :: BExp -> Set Id
getVarFromBExp (CTrue) =  empty
getVarFromBExp (CFalse) = empty
getVarFromBExp (Not n) = getVarFromBExp n
getVarFromBExp (And b1 b2) = getVarFromBExp b1 `union` getVarFromBExp b2
getVarFromBExp (Or b1 b2) = getVarFromBExp b1 `union` getVarFromBExp b2
getVarFromBExp (EQExp b1 b2) = getVarFromAExp b1 `union` getVarFromAExp b2
getVarFromBExp (GTExp b1 b2) = getVarFromAExp b1 `union` getVarFromAExp b2
getVarFromBExp (LTExp b1 b2) = getVarFromAExp b1 `union` getVarFromAExp b2

assigments :: Stmt -> Set (Id, Label)
assigments (Assignment var _ l) = singleton (var, l)
assigments (Skip {}) = empty
assigments (Seq s1 s2) = assigments s1 `union` assigments s2
assigments (IfThenElse _ s1 s2) = assigments s1 `union` assigments s2
assigments (While _ s) =  assigments s

nonTrivialExpression :: Program -> Set (AExp)
nonTrivialExpression (Assignment _ aexp _) = nonTrivialAExpression(aexp)
nonTrivialExpression (Skip _) = empty
nonTrivialExpression (Seq s1 s2) = nonTrivialExpression s1 `union` nonTrivialExpression s2
nonTrivialExpression (IfThenElse (bexp, _) s1 s2) = nonTrivialBExpression bexp `union` nonTrivialExpression s1 `union` nonTrivialExpression s2
nonTrivialExpression (While (bexp, _) s) =  nonTrivialBExpression bexp `union` nonTrivialExpression s

nonTrivialAExpression :: AExp -> Set (AExp)
nonTrivialAExpression aexp = do
    case aexp of
        Add l r -> singleton aexp `union` nonTrivialAExpression l `union` nonTrivialAExpression r
        Sub l r -> singleton aexp `union` nonTrivialAExpression l `union` nonTrivialAExpression r
        Mult l r -> singleton aexp `union` nonTrivialAExpression l `union` nonTrivialAExpression r
        _ -> empty

nonTrivialBExpression :: BExp -> Set (AExp)
nonTrivialBExpression (CTrue) =  empty
nonTrivialBExpression (CFalse) = empty
nonTrivialBExpression (Not _) =  empty
nonTrivialBExpression (And _ _) =  empty
nonTrivialBExpression (Or _ _) =  empty
nonTrivialBExpression (EQExp l r) =  nonTrivialAExpression l `union` nonTrivialAExpression r
nonTrivialBExpression (GTExp l r) =  nonTrivialAExpression l `union` nonTrivialAExpression r
nonTrivialBExpression (LTExp l r) =  nonTrivialAExpression l `union` nonTrivialAExpression r

chaoticIteration :: Eq t => (t -> t) -> t -> t
chaoticIteration f x = if f x == x then x else chaoticIteration f (f x)

intersections :: (Ord a) => [Set a] -> Set a
intersections = Data.List.foldl1' Data.Set.intersection

-- instance Show Set where
--     show s =
--         let xs = [(show x) | x <- Data.Set.toList s]
--         in "{" ++ (intercalate "," xs) ++ "}"