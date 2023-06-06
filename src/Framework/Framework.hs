module Framework.Framework where

import Prelude hiding (init)

import Syntax

import CFG

import Data.Set

type Analysis a = Label -> a

type Transfer a = Label -> Program -> a -> a

data Direction = Forwards | Backwards

data MF a = MF
    { getExtremalValue   :: a                          -- iota
    , getExtremalLabels  :: Set Label                  -- {init} or final
    , getFlow            :: Set (Label, Label)         -- flow or flowR
    , getLattice         :: Lattice a                  -- lattice on property space
    , getTransferFuncion :: Transfer a                 -- transfer function
    , getDirection       :: Direction                  -- direction of the analysis
    }

-- join can be either union or intersection
data Lattice a = Lattice
    { joinOperation  :: a -> a -> a
    , unit           :: a               -- perpendicular symbol
    }

joinAll :: Lattice a -> [a] -> a
joinAll l = Prelude.foldr (joinOperation l) (unit l)
  
forwards :: Program -> MF a -> MF a
forwards prog mf = mf
    { getExtremalLabels = singleton (init prog)
    , getFlow           = flow prog
    , getDirection      = Forwards
    }

backwards :: Program -> MF a -> MF a
backwards prog mf = mf
    { getExtremalLabels = final prog
    , getFlow           = flowR prog
    , getDirection      = Backwards
    }

isForwards :: MF a -> Bool
isForwards mf = case getDirection mf of
    Forwards -> True
    _ -> False

type Kill a = Blocks -> Program -> a
  
type Gen a = Blocks -> Program -> a
  
distributive :: (Ord a) => Kill (Set a) -> Gen (Set a) -> MF (Set a) -> MF (Set a)
distributive kill gen mf = mf { getTransferFuncion = transfer }
    where
    transfer l prog rprog = (rprog `difference` killed) `union` generated
        where
        Just b = findBlock l prog
        killed = kill b prog
        generated = gen b prog

initFramework :: MF a
initFramework = MF
    { getExtremalValue   = error "uninitialized property 'Extremal Value'"
    , getExtremalLabels  = error "uninitialized property 'Extremal Labels'"
    , getFlow            = error "uninitialized property 'Flow'"
    , getLattice         = error "uninitialized property 'Lattice'"
    , getTransferFuncion = error "uninitialized property 'Transfer Function'"
    , getDirection       = error "uninitialized property 'Direction'"
    }