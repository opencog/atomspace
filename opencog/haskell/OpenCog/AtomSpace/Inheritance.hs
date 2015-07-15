-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE EmptyDataDecls, StandaloneDeriving, DeriveDataTypeable,
             FlexibleInstances, DataKinds, KindSignatures, TypeFamilies,
             TypeOperators, ConstraintKinds, UndecidableInstances #-}

-- | This Module defines the relation between different atom types.
module OpenCog.AtomSpace.Inheritance (
   Is
  ) where

import GHC.Exts                     (Constraint)
import OpenCog.AtomSpace.AtomType   (AtomType(..),Up(..))

-- | 'In' type level function to check if a type belongs to a list.
type family In a (b :: [AtomType]) :: Bool where
    In a (a ': b) = 'True
    In a (b ': c) = In a c
    In a '[]      = 'False

-- | 'FUp' type level function to get the list of all the ancestors
-- of a given atom type.
type family FUp a b :: [AtomType] where
    FUp (x ': xs) a         = x ': FUp xs (x ': a)
    FUp '[]       (x ': xs) = FUp (Up x) xs
    FUp '[]       '[]       = '[]

-- | 'IsParent'' is a predicate to decide if atom type b is an ancestor
-- of atom type a.
type family IsParent' a b :: Bool where
    IsParent' a b = (In b (FUp '[a] '[]))

-- | 'IsParent' is a contraint on being 'b' an ancestor of 'a'.
type IsParent a b = IsParent' a b ~ 'True

-- | 'ParConst' builds a list of constraints to assert that all the members of
-- the list are ancestors of a.
type family ParConst a (b :: [AtomType]) :: Constraint where
    ParConst a '[]      = 'True ~ 'True
    ParConst a (b ': c) = (IsParent a b,ParConst a c)


-- | 'Is' builds a list of constraints to assert that all the ancestors of b
-- (included b itself) are ancestors of a.
type Is a b = ParConst a (FUp '[b] '[])

