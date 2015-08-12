-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | This Module defines atom filters based on their hierarchy.
module OpenCog.AtomSpace.Filter (
    Gen(..)
  , appGen
  , FilterIsChild(..)
  ) where

import OpenCog.AtomSpace.Template       (atomHierarchyFile,declareAtomFilters)
import OpenCog.AtomSpace.Inheritance    (type (<~))
import OpenCog.AtomSpace.AtomType       (AtomType(..))
import OpenCog.AtomSpace.Types          (Atom(..),Gen(..),appGen)
import Data.Proxy                       (Proxy(..))
import Data.Typeable                    (cast,Typeable,typeRep)
import Data.Functor                     ((<$>))

-- | 'FilterIsChild' class defines a filter on the descendants of atom type 'a'.
class FilterIsChild a where
    filtIsChild :: (b <~ AtomT) => Atom b -> Maybe (Gen a)

getPhantomType :: (Typeable a) => Atom a -> AtomType
getPhantomType = read . show . typeRep

-- Usage of Template Haskell to generate instances of FilterIsChild for each
-- Atom Type.
declareAtomFilters [atomHierarchyFile|../atomspace/atom_types.script|]
