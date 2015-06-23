-- GSoC 2015 - Haskell bindings for OpenCog.

-- | This library defines Haskell Bindings for the AtomSpace.
module OpenCog.AtomSpace
    (
    -- * AtomSpace Environment
      AtomSpace
    , runOnNewAtomSpace
    -- * AtomSpace Interaction
    , insert
    , remove
    , get
    , debug
    -- * AtomSpace Printing
    , printAtom
    , showAtom
    -- * AtomSpace Main Data Types
    , TruthVal (..)
    , AtomName (..)
    , Atom (..)
    , AtomGen (..)
    , appAtomGen
    ) where

import OpenCog.AtomSpace.Api
import OpenCog.AtomSpace.Types
import OpenCog.AtomSpace.Env        (AtomSpace,runOnNewAtomSpace)
import OpenCog.AtomSpace.Utils      (printAtom,showAtom)

