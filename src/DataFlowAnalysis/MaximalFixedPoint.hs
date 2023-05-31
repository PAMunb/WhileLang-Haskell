module DataFlowAnalysis.MaximalFixedPoint where

import Syntax

import CFG

import DataFlowAnalysis.Helpers
import DataFlowAnalysis.ReachingDefinitions
import DataFlowAnalysis.LiveVariables
import DataFlowAnalysis.VeryBusyExpressions
import DataFlowAnalysis.AvailableExpressions

import Data.Set

data AnalysisOutputType = RDOut DataFlowAnalysis.ReachingDefinitions.RD
                        | LVOut DataFlowAnalysis.LiveVariables.LV
                        | AEOut DataFlowAnalysis.AvailableExpressions.AE
                        | VBOut DataFlowAnalysis.VeryBusyExpressions.VB

data Analysis = RD DataFlowAnalysis.ReachingDefinitions.RDEntry
              | LV DataFlowAnalysis.LiveVariables.LVExit
              | AE DataFlowAnalysis.AvailableExpressions.AEEntry
              | VB DataFlowAnalysis.VeryBusyExpressions.VBExit

data AnalysisInverse = RDInv DataFlowAnalysis.ReachingDefinitions.RDExit
                     | LVInv DataFlowAnalysis.LiveVariables.LVEntry
                     | AEInv DataFlowAnalysis.AvailableExpressions.AEExit
                     | VBInv DataFlowAnalysis.VeryBusyExpressions.VBEntry

getRD :: AnalysisOutputType -> DataFlowAnalysis.ReachingDefinitions.RD
getRD (RDOut rd) = rd
getRD _ = empty

getLV :: AnalysisOutputType -> DataFlowAnalysis.LiveVariables.LV
getLV (LVOut lv) = lv
getLV _ = empty

getAE :: AnalysisOutputType -> DataFlowAnalysis.AvailableExpressions.AE
getAE (AEOut ae) = ae
getAE _ = empty

getVB :: AnalysisOutputType -> DataFlowAnalysis.VeryBusyExpressions.VB
getVB (VBOut vb) = vb
getVB _ = empty

external :: Program -> Analysis -> Set Label
external p (RD _) = Data.Set.singleton (CFG.init p)
external p (LV _) = CFG.final p
external p (AE _) = Data.Set.singleton (CFG.init p)
external p (VB _) = CFG.final p

flow :: Program -> Analysis -> Set (Label, Label)
flow p (RD _) = CFG.flow p
flow p (LV _) = CFG.flowR p
flow p (AE _) = CFG.flow p
flow p (VB _) = CFG.flowR p

lFunc :: Program -> Analysis -> AnalysisOutputType
lFunc p (RD _) = RDOut (Data.Set.fromList [(v, DataFlowAnalysis.ReachingDefinitions.uninitialized) | v <- Data.Set.toList (DataFlowAnalysis.Helpers.fv p) ])
lFunc _ _= LVOut (Data.Set.empty)

fFunc :: Label -> Program -> Analysis -> AnalysisOutputType
fFunc l p (RD rd) = RDOut (DataFlowAnalysis.ReachingDefinitions.rdExit l p rd)
fFunc l p (LV lv) = LVOut (DataFlowAnalysis.LiveVariables.lvEntry l p lv)
fFunc l p (AE ae) = AEOut (DataFlowAnalysis.AvailableExpressions.aeExit l p ae)
fFunc l p (VB vb) = VBOut (DataFlowAnalysis.VeryBusyExpressions.vbEntry l p vb)

analysis :: Label -> Program -> AnalysisInverse -> AnalysisOutputType
analysis l p (RDInv rd) = RDOut (DataFlowAnalysis.ReachingDefinitions.rdEntry l p rd)
analysis l p (LVInv lv) = LVOut (DataFlowAnalysis.LiveVariables.lvExit l p lv)
analysis l p (AEInv ae) = AEOut (DataFlowAnalysis.AvailableExpressions.aeEntry l p ae)
analysis l p (VBInv vb) = VBOut (DataFlowAnalysis.VeryBusyExpressions.vbExit l p vb)

-- initializeMFP :: Set (Label, Label) -> Set Label -> Analysis
