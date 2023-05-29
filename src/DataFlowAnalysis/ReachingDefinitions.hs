module DataFlowAnalysis.ReachingDefinitions where

import Syntax
import CFG
import DataFlowAnalysis.Helpers

import Data.Set
import Data.Map

uninitialized :: Label
uninitialized = -1 -- equivalent to ? from the book

type RD = Set (Id, Label)
type RDEntry = Data.Map.Map Label RD
type RDExit = Data.Map.Map Label RD

reachingDefinitions :: Program -> (RDEntry, RDExit)
reachingDefinitions prog = chaoticIteration f initial
  where
    initial = (Data.Map.empty, Data.Map.empty)
    f rd = Data.Set.foldr updateMappings rd (labels prog)
    updateMappings l (entry', exit') =
      let 
          newEntry = rdEntry l prog exit'
          newExit = rdExit l prog entry'
      in (Data.Map.insert l newEntry entry', Data.Map.insert l newExit exit')

  
killRD :: Blocks -> Program -> RD
killRD (BlocksStmt blcks) prog = do
    case blcks of
        Assignment var _ l -> Data.Set.singleton (var, uninitialized) `Data.Set.union` Data.Set.filter (\(v, l') -> v == var && l' /= l) (assigments prog)
        _ -> Data.Set.empty
killRD (BlocksTest {}) _ = Data.Set.empty

genRD :: Blocks -> Program -> RD
genRD (BlocksStmt stm) _ = do
    case stm of
        Assignment var _ l -> Data.Set.singleton (var, l)
        _ -> Data.Set.empty
genRD (BlocksTest {}) _ = Data.Set.empty

rdEntry :: Label -> Program -> RDExit  -> RD
rdEntry l prog exit
  | l == CFG.init prog = Data.Set.fromList [(v, uninitialized) | v <- Data.Set.toList (fv prog)]
  | otherwise = Data.Set.unions [findWithDefault Data.Set.empty l' exit | (l', label) <- Data.Set.toList(flow prog), label == l]

rdExit :: Label -> Program -> RDEntry -> RD
rdExit l prog entry = do
  let Just b = findBlock l prog
  ((findWithDefault Data.Set.empty l entry) `Data.Set.difference` (killRD b prog)) `Data.Set.union` (genRD b prog)
  