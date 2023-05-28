module DataFlowAnalysis.LiveVariables where

import Syntax
import CFG
import DataFlowAnalysis.DFA

import Data.Set
import Data.Map

type LV = Set (Id)
type LVEntry = Data.Map.Map Label LV
type LVExit = Data.Map.Map Label LV

liveVariable :: Program -> (LVEntry, LVExit)
liveVariable prog = chaoticIteration f initial
  where
    initial = (Data.Map.empty, Data.Map.empty)
    f lv = Data.Set.foldr updateMappings lv (labels prog)
    updateMappings l (entry', exit') =
      let 
          newEntry = lvEntry l prog exit'
          newExit = lvExit l prog entry'
      in (Data.Map.insert l newEntry entry', Data.Map.insert l newExit exit')

killLV :: Blocks -> LV
killLV (BlocksStmt blcks) = do
    case blcks of
        Assignment var _ _ -> Data.Set.singleton (var)
        _ -> Data.Set.empty
killLV (BlocksTest _) = Data.Set.empty

genLV :: Blocks -> LV
genLV (BlocksStmt stm) = do
    case stm of
        Assignment _ aexp _ -> getVarFromAExp aexp
        _ -> Data.Set.empty
genLV (BlocksTest (texp, _)) = getVarFromBExp texp

lvExit :: Label -> Program -> LVEntry -> LV
lvExit l prog entry
  | l `Data.Set.member` CFG.final prog = Data.Set.empty
  | otherwise = Data.Set.unions [findWithDefault Data.Set.empty l' entry | (l', label) <- Data.Set.toList(flowR prog), label == l]

lvEntry :: Label -> Program -> LVExit -> LV
lvEntry l prog exit = do
  let Just b = findBlock l prog
  ((findWithDefault Data.Set.empty l exit) `Data.Set.difference` (killLV b)) `Data.Set.union` (genLV b)