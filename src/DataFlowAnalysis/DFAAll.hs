module DataFlowAnalysis.DFAAll where

import Syntax
import CFG
import DataFlowAnalysis.Helpers
import DataFlowAnalysis.LiveVariables
import DataFlowAnalysis.ReachingDefinitions
import DataFlowAnalysis.AvailableExpressions
import DataFlowAnalysis.VeryBusyExpressions
import DataFlowAnalysis.Framework

import Data.Set
import Data.Map

dfa :: Program -> (Analysis, AnalysisInverse) -> (AnalysisOutputType, AnalysisOutputType)
dfa prog initial = chaoticIteration f initial
  where
    f mappings = Data.Set.foldr updateMappings mappings (labels prog)
    updateMappings l (analysis', analysisInv') =
      let
          newAnalysisItem = analysis l prog analysisInv'
          newAnalysisInvItem = fFunc l prog analysis'
          newAnalysisOut = Data.Map.insert l newAnalysisItem
          newAnalysisInvOut = Data.Map.insert l newAnalysisInvItem
      in (newAnalysisOut Data.Map.union analysis', newAnalysisInvOut Data.Map.union analysisInv')

dfa_reachingDefinitions :: Program -> (RDEntry, RDExit)
dfa_reachingDefinitions prog = getRD (dfa prog (initialize (external prog (RD Data.Map.empty)) (lFunc prog (RD Data.Map.empty))) (emptyAnalysisInverse (RDInv Data.Map.empty)))

-- dfa_liveVariables :: Program -> (LVEntry, LVExit)
-- dfa_liveVariables prog = dfa prog genLV killLV False True
-- 
-- dfa_availableExpressions :: Program -> (AEEntry, AEExit)
-- dfa_availableExpressions prog = dfa prog genAE killAE True False
-- 
-- dfa_veryBusyExpressions :: Program -> (VBEntry, VBExit)
-- dfa_veryBusyExpressions prog = dfa prog genVB killVB False False

-- combinator :: (Ord a) => Label -> Program -> (Data.Map.Map Label (Set a)) -> Bool -> Bool -> Set a
-- combinator l prog mapToCombine isForward isMay
--   | l == CFG.init prog = Data.Set.empty
--   | otherwise = if isMay then Data.Set.unions sets else intersections sets where
--     cfg = if isForward then flow prog else flowR prog
--     sets = [findWithDefault Data.Set.empty l' mapToCombine | (l', label) <- Data.Set.toList(cfg), label == l]
-- 
-- generator :: (Ord a) => Label -> Program -> (Data.Map.Map Label (Set a)) -> (Blocks -> Program -> (Set a)) -> (Blocks -> Program -> (Set a)) -> Set a
-- generator l prog mapToCombine gen kill = do
--     let Just b = findBlock l prog
--     ((findWithDefault Data.Set.empty l mapToCombine) `Data.Set.difference` (kill b prog)) `Data.Set.union` (gen b prog)

