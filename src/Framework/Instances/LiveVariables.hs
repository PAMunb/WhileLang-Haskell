module Framework.Instances.LiveVariables where

import Framework.Framework
import Syntax
import Data.Set

import DataFlowAnalysis.LiveVariables

-- type LV = Id
mfLV :: Program -> MF (Set LV)
mfLV prog
  = backwards prog
  $ distributive killLV genLV
  $ initFramework
  { getExtremalValue = empty
  , getLattice = Lattice
    { joinOperation = union
    , unit = empty
    }
  }