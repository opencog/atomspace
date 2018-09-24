-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE TypeOperators #-}

-- | This library defines Haskell Bindings for the AtomSpace.
module OpenCog.AtomSpace
    (
    -- * AtomSpace Environment
      AtomSpace
    , AtomSpaceObj
    , getParent
    , newAtomSpace
    , onAtomSpace
    , (<:)
    , runOnNewAtomSpace
    , refToObj
    -- * AtomSpace Interaction
    , insert
    , insertAndGetHandle
    , remove
    , get
    , getByHandle
    , debug
    -- * AtomSpace Execution
    , execute
    , evaluate
    -- * AtomSpace Query
    , module OpenCog.AtomSpace.Query
    -- * AtomSpace Printing
    , printAtom
    , showAtom
    -- * AtomSpace Main Data Types
    , TruthVal (..)
    , AtomName (..)
    , Atom (..)
    -- * AtomSpace Syntactic Sugar
    , module OpenCog.AtomSpace.Sugar
    -- * Function for use in GSN
    , exportFunction
    , exportPredicate
    , Handle
    , HandleSeq
    , TruthValueP
    , AtomSpaceRef
    -- * Utility Functions for working with Atoms
    , atomMap
    , atomMapM
    , atomFold
    , atomElem
    , nodeName
    , atomType
    , atomGetAllNodes
    ) where

import OpenCog.AtomSpace.Api
import OpenCog.AtomSpace.Types
import OpenCog.AtomSpace.Env
import OpenCog.AtomSpace.Utils
import OpenCog.AtomSpace.Sugar
import OpenCog.AtomSpace.Query
import OpenCog.AtomSpace.Internal    (Handle,HandleSeq,TruthValueP)
