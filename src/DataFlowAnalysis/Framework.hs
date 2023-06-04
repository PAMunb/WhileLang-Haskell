module DataFlowAnalysis.Framework where

import Syntax

import CFG

import DataFlowAnalysis.Helpers
import DataFlowAnalysis.ReachingDefinitions
import DataFlowAnalysis.LiveVariables
import DataFlowAnalysis.VeryBusyExpressions
import DataFlowAnalysis.AvailableExpressions

import Data.Set
import Data.Map

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

-- getRDOut :: AnalysisOutputType -> DataFlowAnalysis.ReachingDefinitions.RD
-- getRDOut (RDOut rd) = rd
-- getRDOut _ = Data.Set.empty

getRD :: (Analysis, AnalysisInverse) -> (DataFlowAnalysis.ReachingDefinitions.RDEntry, DataFlowAnalysis.ReachingDefinitions.RDExit)
getRD (RD rd, RDInv rdInv) = (rd, rdInv)
getRD _ = (Data.Map.empty, Data.Map.empty)

getLV :: AnalysisOutputType -> DataFlowAnalysis.LiveVariables.LV
getLV (LVOut lv) = lv
getLV _ = Data.Set.empty

getAE :: AnalysisOutputType -> DataFlowAnalysis.AvailableExpressions.AE
getAE (AEOut ae) = ae
getAE _ = Data.Set.empty

getVB :: AnalysisOutputType -> DataFlowAnalysis.VeryBusyExpressions.VB
getVB (VBOut vb) = vb
getVB _ = Data.Set.empty

initialize :: Set Label -> AnalysisOutputType -> Analysis
initialize ext (RDOut initial) = (RD (Data.Map.fromList [(k, initial) | k <- Data.Set.toList ext]))
initialize ext (LVOut initial) = (LV (Data.Map.fromList [(k, initial) | k <- Data.Set.toList ext]))
initialize ext (AEOut initial) = (AE (Data.Map.fromList [(k, initial) | k <- Data.Set.toList ext]))
initialize ext (VBOut initial) = (VB (Data.Map.fromList [(k, initial) | k <- Data.Set.toList ext]))

analysisConverter :: [(Label, AnalysisOutputType)] -> Analysis
analysisConverter [(l, (RDOut rd))] = (RD (Data.Map.fromList [(l, rd)]))
analysisConverter [(l, (LVOut lv))] = (LV (Data.Map.fromList [(l, lv)]))
analysisConverter [(l, (AEOut ae))] = (AE (Data.Map.fromList [(l, ae)]))
analysisConverter [(l, (VBOut vb))] = (VB (Data.Map.fromList [(l, vb)]))

analysisInvConverter :: [(Label, AnalysisOutputType)] -> AnalysisInverse
analysisInvConverter [(l, (RDOut rd))] = (RDInv (Data.Map.fromList [(l, rd)]))
analysisInvConverter [(l, (LVOut lv))] = (LVInv (Data.Map.fromList [(l, lv)]))
analysisInvConverter [(l, (AEOut ae))] = (AEInv (Data.Map.fromList [(l, ae)]))
analysisInvConverter [(l, (VBOut vb))] = (VBInv (Data.Map.fromList [(l, vb)]))

emptyAnalysisInverse :: AnalysisInverse -> AnalysisInverse
emptyAnalysisInverse (RDInv _) = (RDInv Data.Map.empty)
emptyAnalysisInverse (LVInv _) = (LVInv Data.Map.empty)
emptyAnalysisInverse (AEInv _) = (AEInv Data.Map.empty)
emptyAnalysisInverse (VBInv _) = (VBInv Data.Map.empty)

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
lFunc _ (LV _)= LVOut Data.Set.empty
lFunc _ (AE _)= AEOut Data.Set.empty
lFunc _ (VB _)= VBOut Data.Set.empty

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

