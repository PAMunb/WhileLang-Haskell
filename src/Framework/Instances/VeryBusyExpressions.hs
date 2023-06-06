module Framework.Instances.VeryBusyExpressions where

import Framework.Framework
import Syntax
import DataFlowAnalysis.Helpers
import Data.Set

import DataFlowAnalysis.VeryBusyExpressions

-- type VB = Set (AExp)
mfVB :: Program -> MF (Set VB)
mfVB prog
  = backwards prog
  $ distributive killVB genVB
  $ initFramework
  { getExtremalValue = Data.Set.empty
  , getLattice = Lattice
    { joinOperation = intersection
    , unit = nonTrivialExpression(prog)
    }
  }