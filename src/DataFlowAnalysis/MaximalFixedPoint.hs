module DataFlowAnalysis.MaximalFixedPoint where

import Syntax
import CFG

import DataFlowAnalysis.ReachingDefinitions
import DataFlowAnalysis.LiveVariables

import Data.Set

data Analysis = RD DataFlowAnalysis.ReachingDefinitions.RD
              | LV DataFlowAnalysis.LiveVariables.LV

data AnalysisEntry = RDEntry DataFlowAnalysis.ReachingDefinitions.RDEntry
                   | LVEntry DataFlowAnalysis.LiveVariables.LVEntry

data AnalysisExit = RDExit
                  | LVExit

getRD :: Analysis -> RD
getRD (RD rd) = rd
getRD (LV _) = empty

getLV :: Analysis -> LV
getLV (RD _) = empty
getLV (LV lv) = lv

mapping :: Label -> Program -> AnalysisEntry -> Analysis
mapping l p (RDEntry rd) = RD (DataFlowAnalysis.ReachingDefinitions.rdExit l p rd)
mapping l p (LVEntry lv) = LV (DataFlowAnalysis.LiveVariables.lvExit l p lv)

lFunc :: Maybe Program -> Analysis
lFunc (Just p) = RD (Data.Set.fromList [(v, DataFlowAnalysis.ReachingDefinitions.uninitialized) | v <- Data.Set.toList (CFG.getVars p) ])
lFunc Nothing = LV (Data.Set.empty)

-- initializeMFP :: Set (Label, Label) -> Set Label -> Analysis
