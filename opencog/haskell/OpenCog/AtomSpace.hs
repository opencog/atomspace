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
    -- * AtomSpace Interaction
    , insert
    , remove
    , get
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
    , UUID
    , AtomSpaceRef
    ) where

import OpenCog.AtomSpace.Api
import OpenCog.AtomSpace.Types
import OpenCog.AtomSpace.Env         (AtomSpace(..),runOnNewAtomSpace,AtomSpaceObj,
                                      getParent,newAtomSpace,onAtomSpace,(<:),
                                      AtomSpaceRef(..))
import OpenCog.AtomSpace.Utils       (printAtom,showAtom)
import OpenCog.AtomSpace.Sugar
import OpenCog.AtomSpace.Query
import OpenCog.AtomSpace.Internal    (UUID)
