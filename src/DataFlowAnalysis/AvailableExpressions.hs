module DataFlowAnalysis.AvailableExpressions where

import Syntax
import CFG
import DataFlowAnalysis.DFA

import Data.Set
import Data.Map

type AE = Set (AExp)
type AEEntry = Data.Map.Map Label AE
type AEExit = Data.Map.Map Label AE

availableExpressions :: Program -> (AEEntry, AEExit)
availableExpressions prog = chaoticIteration update initial
  where
    initial = (Data.Map.empty, Data.Map.empty)
    update ae = Data.Set.foldr updateMappings ae (labels prog)
    updateMappings l (entry', exit') =
      let 
          newEntry = aeEntry l prog exit'
          newExit = aeExit l prog entry'
      in (Data.Map.insert l newEntry entry', Data.Map.insert l newExit exit')

killAE :: Blocks -> Program -> AE
killAE (BlocksStmt blcks) prog = do
    case blcks of
        Assignment var _ _ -> Data.Set.fromList [ a' | a' <- Data.Set.toList(nonTrivialExpression(prog)), var `Data.Set.member` fvAExp(a')]
        _ -> Data.Set.empty
killAE (BlocksTest _) _ = Data.Set.empty

genAE :: Blocks -> Program -> AE
genAE (BlocksStmt blcks) prog = do
    case blcks of
        Assignment var aexp _ -> Data.Set.fromList [ a' | a' <- Data.Set.toList(nonTrivialAExpression(aexp)), var `Data.Set.notMember` fvAExp(a')]
        _ -> Data.Set.empty
genAE (BlocksTest (bexp, _)) _ = nonTrivialBExpression bexp

aeEntry :: Label -> Program -> AEExit -> AE
aeEntry l prog exit
  | l == CFG.init prog = Data.Set.empty
  | otherwise = intersections [findWithDefault Data.Set.empty l' exit | (l', label) <- Data.Set.toList(flow prog), label == l]

aeExit :: Label -> Program -> AEEntry -> AE
aeExit l prog entry = do
    let Just b = findBlock l prog
    ((findWithDefault Data.Set.empty l entry) `Data.Set.difference` (killAE b prog)) `Data.Set.union` (genAE b prog)
