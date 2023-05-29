module DataFlowAnalysis.VeryBusyExpressions where

import Syntax
import CFG
import DataFlowAnalysis.Helpers

import Data.Set
import Data.Map

type VB = Set (AExp)
type VBEntry = Data.Map.Map Label VB
type VBExit = Data.Map.Map Label VB

veryBusyExpressions :: Program -> (VBEntry, VBExit)
veryBusyExpressions prog = chaoticIteration f initial
  where
    initial = (Data.Map.empty, Data.Map.empty)
    f vb = Data.Set.foldr updateMappings vb (labels prog)
    updateMappings l (entry', exit') =
      let 
          newEntry = vbEntry l prog exit'
          newExit = vbExit l prog entry'
      in (Data.Map.insert l newEntry entry', Data.Map.insert l newExit exit')

killVB :: Blocks -> Program -> VB
killVB (BlocksStmt blcks) prog = do
    case blcks of
        Assignment var _ _ -> Data.Set.fromList [ a' | a' <- Data.Set.toList(nonTrivialExpression(prog)), var `Data.Set.member` fvAExp(a')]
        _ -> Data.Set.empty
killVB (BlocksTest _) _ = Data.Set.empty

genVB :: Blocks -> Program -> VB
genVB (BlocksStmt blcks) _ = do
    case blcks of
        Assignment _ aexp _ -> nonTrivialAExpression aexp
        _ -> Data.Set.empty
genVB (BlocksTest (bexp, _)) _ = nonTrivialBExpression bexp

vbExit :: Label -> Program -> VBEntry -> VB
vbExit l prog entry
  | l `Data.Set.member` CFG.final prog = Data.Set.empty
  | otherwise = intersections [findWithDefault Data.Set.empty l' entry | (l', label) <- Data.Set.toList(flowR prog), label == l]

vbEntry :: Label -> Program -> VBExit -> VB
vbEntry l prog exit = do
    let Just b = findBlock l prog
    ((findWithDefault Data.Set.empty l exit) `Data.Set.difference` (killVB b prog)) `Data.Set.union` (genVB b prog)
