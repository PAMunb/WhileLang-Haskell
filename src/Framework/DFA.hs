module Framework.DFA where

import Framework.Framework
import Syntax
import CFG
import DataFlowAnalysis.Helpers
import Data.Set
import Data.Map

dfa :: (Ord a) => (Program -> MF (Set a)) -> Program -> (Data.Map.Map Label (Set a), Data.Map.Map Label (Set a))
dfa mfd prog = chaoticIteration f initial
  where
    mf = mfd prog
    initial = (Data.Map.empty, Data.Map.empty)
    f mappings = Data.Set.foldr updateMappings mappings (labels prog)
    updateMappings l (entry', exit') =
        let 
            newEntry = if (isForwards mf) then analysis l exit' else analysisT l exit'
            newExit = if (isForwards mf) then analysisT l entry' else analysis l entry'
        in (Data.Map.insert l newEntry entry', Data.Map.insert l newExit exit')
            where
                analysis lab t = joinOperation (getLattice mf) (joinAll (getLattice mf) [findWithDefault Data.Set.empty l' t | (l', label) <- Data.Set.toList(getFlow mf), label == lab]) extremal
                    where 
                        extremal | l `Data.Set.member` (getExtremalLabels mf) = (getExtremalValue mf)
                                 | otherwise = (unit (getLattice mf))
                analysisT lab t = (getTransferFuncion mf) lab prog (findWithDefault Data.Set.empty lab t)
        

