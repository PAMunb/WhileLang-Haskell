module DataFlowAnalysis.LiveVariables where

import Syntax
import CFG
import DataFlowAnalysis.Helpers

import Data.Set
import Data.Map

type LV = Id
type LVEntry = Data.Map.Map Label (Set LV)
type LVExit = Data.Map.Map Label (Set LV)

liveVariables :: Program -> (LVEntry, LVExit)
liveVariables prog = chaoticIteration f initial
  where
    initial = (Data.Map.empty, Data.Map.empty)
    f lv = Data.Set.foldr updateMappings lv (labels prog)
    updateMappings l (entry', exit') =
      let 
          newEntry = lvEntry l prog exit'
          newExit = lvExit l prog entry'
      in (Data.Map.insert l newEntry entry', Data.Map.insert l newExit exit')

killLV :: Blocks -> Program -> Set LV
killLV (BlocksStmt blcks) _ = do
    case blcks of
        Assignment var _ _ -> Data.Set.singleton (var)
        _ -> Data.Set.empty
killLV (BlocksTest _) _ = Data.Set.empty

genLV :: Blocks -> Program -> Set LV
genLV (BlocksStmt stm) _ = do
    case stm of
        Assignment _ aexp _ -> getVarFromAExp aexp
        _ -> Data.Set.empty
genLV (BlocksTest (texp, _)) _ = getVarFromBExp texp

lvExit :: Label -> Program -> LVEntry -> Set LV
lvExit l prog entry
  | l `Data.Set.member` CFG.final prog = Data.Set.empty
  | otherwise = Data.Set.unions [findWithDefault Data.Set.empty l' entry | (l', label) <- Data.Set.toList(flowR prog), label == l]

lvEntry :: Label -> Program -> LVExit -> Set LV
lvEntry l prog exit = do
  let Just b = findBlock l prog
  ((findWithDefault Data.Set.empty l exit) `Data.Set.difference` (killLV b prog)) `Data.Set.union` (genLV b prog)