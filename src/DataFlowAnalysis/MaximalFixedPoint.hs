module DataFlowAnalysis.MaximalFixedPoint where

import Syntax
import CFG

import DataFlowAnalysis.ReachingDefinitions
import DataFlowAnalysis.LiveVariables

import Data.Set

data Analisys = RD DataFlowAnalysis.ReachingDefinitions.RD
              | LV DataFlowAnalysis.LiveVariables.LV

data AnalisysEntry = RDEntry DataFlowAnalysis.ReachingDefinitions.RDEntry
                   | LVEntry DataFlowAnalysis.LiveVariables.LVEntry

data AnalisysExit = RDExit
                  | LVExit

mapping :: Label -> Program -> AnalisysEntry -> Analisys
mapping l p (RDEntry rd) = RD (DataFlowAnalysis.ReachingDefinitions.rdExit l p rd)
mapping l p (LVEntry lv) = LV (DataFlowAnalysis.LiveVariables.lvExit l p lv)

lFunc :: Maybe Program -> RD
lFunc (Just p) = Data.Set.fromList [(v, DataFlowAnalysis.ReachingDefinitions.uninitialized) | v <- Data.Set.toList (CFG.getVars p) ]
lFunc Nothing = Data.Set.empty

-- initializeMFP :: Set (Label, Label) -> Set Label -> Analisys
