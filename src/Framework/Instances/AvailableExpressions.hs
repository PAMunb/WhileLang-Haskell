module Framework.Instances.AvailableExpressions where

import Framework.Framework
import Syntax
import DataFlowAnalysis.Helpers
import Data.Set

import DataFlowAnalysis.AvailableExpressions

-- type AE = Set (AExp)
mfAE :: Program -> MF (Set AE)
mfAE prog
  = forwards prog
  $ distributive killAE genAE
  $ initFramework
  { getExtremalValue = Data.Set.empty
  , getLattice = Lattice
    { joinOperation = intersection
    , unit = nonTrivialExpression(prog)
    }
  }