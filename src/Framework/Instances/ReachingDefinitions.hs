module Framework.Instances.ReachingDefinitions where

import Framework.Framework
import Syntax
import DataFlowAnalysis.Helpers
import Data.Set

import DataFlowAnalysis.ReachingDefinitions

-- type RD = (Id, Label)
mfRD :: Program -> MF (Set RD)
mfRD prog
  = forwards prog
  $ distributive killRD genRD
  $ initFramework
  { getExtremalValue = Data.Set.fromList [(v, uninitialized) | v <- Data.Set.toList (fv prog)]
  , getLattice = Lattice
    { joinOperation = union
    , unit = empty
    }
  }