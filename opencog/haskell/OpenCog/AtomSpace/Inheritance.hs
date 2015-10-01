-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This Module defines the relation between different atom types.
module OpenCog.AtomSpace.Inheritance (
  type (<~)
) where

import GHC.Exts                     (Constraint)
import OpenCog.AtomSpace.AtomType   (AtomType(..),Is(..),Up(..))
import Data.Typeable                (Typeable)

-- | 'IsParent' is a contraint on being 'b' an ancestor of 'a'.
type IsParent a b = Is a b ~ 'True

-- | 'ParConst' builds a list of constraints to assert that all the members of
-- the list are ancestors of a.
type family ParConst a (b :: [AtomType]) :: Constraint where
    ParConst a '[]      = 'True ~ 'True
    ParConst a (b ': c) = (IsParent a b,ParConst a c)

-- | '<~' builds a list of constraints to assert that all the ancestors of b
-- (included b itself) are ancestors of a.
infix 9 <~
type a <~ b = (Typeable a,ParConst a (Up b))

