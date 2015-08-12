-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE AutoDeriveTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This Module defines the different Atom Types and some utils functions.
module OpenCog.AtomSpace.AtomType (
    AtomType(..)
  , Up(..)
  , Is(..)
  , fromAtomTypeRaw
  , toAtomTypeRaw
  ) where

import OpenCog.AtomSpace.Template       (atomHierarchyFile,declareAtomType)

-- Usage of Template Haskell to generate definitions of AtomType,
-- some util functions: fromAtomTypeRaw and toAtomTypeRaw,
-- and type families: Up and Is.
declareAtomType [atomHierarchyFile|../atomspace/atom_types.script|]
