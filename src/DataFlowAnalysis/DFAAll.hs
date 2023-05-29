module DataFlowAnalysis.DFAAll where

import Syntax
import CFG
import DataFlowAnalysis.Helpers
import DataFlowAnalysis.LiveVariables
import DataFlowAnalysis.ReachingDefinitions
import DataFlowAnalysis.AvailableExpressions
import DataFlowAnalysis.VeryBusyExpressions

import Data.Set
import Data.Map

dfa :: (Ord a) => Program -> (Blocks -> Program -> (Set a)) -> (Blocks -> Program -> (Set a)) -> Bool -> Bool ->  (Data.Map.Map Label (Set a), Data.Map.Map Label (Set a))
dfa prog gen kill isForward isMay = chaoticIteration f initial
  where
    initial = (Data.Map.empty, Data.Map.empty)
    f mappings = Data.Set.foldr updateMappings mappings (labels prog)
    updateMappings l (entry', exit') =
      let 
          newEntry = if isForward then combinator l prog exit' isForward isMay else  generator l prog exit' gen kill
          newExit = if isForward then generator l prog entry' gen kill else combinator l prog entry' isForward isMay
      in (Data.Map.insert l newEntry entry', Data.Map.insert l newExit exit')

combinator :: (Ord a) => Label -> Program -> (Data.Map.Map Label (Set a)) -> Bool -> Bool -> Set a
combinator l prog mapToCombine isForward isMay
  | l == CFG.init prog = Data.Set.empty
  | otherwise = if isMay then Data.Set.unions sets else intersections sets where
    cfg = if isForward then flow prog else flowR prog
    sets = [findWithDefault Data.Set.empty l' mapToCombine | (l', label) <- Data.Set.toList(cfg), label == l]

generator :: (Ord a) => Label -> Program -> (Data.Map.Map Label (Set a)) -> (Blocks -> Program -> (Set a)) -> (Blocks -> Program -> (Set a)) -> Set a
generator l prog mapToCombine gen kill = do
    let Just b = findBlock l prog
    ((findWithDefault Data.Set.empty l mapToCombine) `Data.Set.difference` (kill b prog)) `Data.Set.union` (gen b prog)

dfa_reachingDefinitions :: Program -> (RDEntry, RDExit)
dfa_reachingDefinitions prog = dfa prog genRD killRD True True

dfa_liveVariables :: Program -> (LVEntry, LVExit)
dfa_liveVariables prog = dfa prog genLV killLV False True

dfa_availableExpressions :: Program -> (AEEntry, AEExit)
dfa_availableExpressions prog = dfa prog genAE killAE True False

dfa_veryBusyExpressions :: Program -> (VBEntry, VBExit)
dfa_veryBusyExpressions prog = dfa prog genVB killVB False False