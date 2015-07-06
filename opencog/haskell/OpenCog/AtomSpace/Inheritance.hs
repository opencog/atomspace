-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE EmptyDataDecls, StandaloneDeriving, DeriveDataTypeable,
             FlexibleInstances, DataKinds, KindSignatures, TypeFamilies,
             TypeOperators, ConstraintKinds, UndecidableInstances #-}

-- | This Module defines the relation between different atom types.
module OpenCog.AtomSpace.Inheritance (
    AtomType(..)
  , Is(..)
  ) where

import GHC.Exts

type family Up a :: [AtomType] where
    Up AtomT           = '[]
    Up NodeT           = '[AtomT]
    Up LinkT           = '[AtomT]
    Up ConceptT        = '[NodeT]
    Up SchemaT         = '[NodeT]
    Up PredicateT      = '[NodeT]
    Up NumberT         = '[NodeT]
    Up GroundedSchemaT = '[SchemaT]
    Up ExecutionT      = '[LinkT]
    Up AndT            = '[LinkT]
    Up OrT             = '[LinkT]
    Up ImplicationT    = '[LinkT]
    Up EquivalenceT    = '[LinkT]
    Up EvaluationT     = '[LinkT]
    Up InheritanceT    = '[LinkT]
    Up SimilarityT     = '[LinkT]
    Up MemberT         = '[LinkT]
    Up SatisfyingSetT  = '[LinkT]
    Up ListT           = '[LinkT]

type family In a (b :: [AtomType]) :: Bool where
    In a (a ': b) = 'True
    In a (b ': c) = In a c
    In a '[]      = 'False

type family FUp a b :: [AtomType] where
    FUp (x ': xs) a         = x ': FUp xs (x ': a)
    FUp '[]       (x ': xs) = FUp (Up x) xs
    FUp '[]       '[]       = '[]

type family IsParent' a b :: Bool where
    IsParent' a b = (In b (FUp '[a] '[]))

type IsParent a b = IsParent' a b ~ 'True

type family ParConst a (b :: [AtomType]) :: Constraint where
    ParConst a '[]      = 'True ~ 'True
    ParConst a (b ': c) = (IsParent a b,ParConst a c)

type Is a b = ParConst a (FUp '[b] '[])

data AtomType = AtomT
              | NodeT
              | LinkT
              | PredicateT
              | ConceptT
              | SchemaT
              | GroundedSchemaT
              | NumberT
              | AndT
              | OrT
              | ImplicationT
              | EquivalenceT
              | EvaluationT
              | InheritanceT
              | SimilarityT
              | MemberT
              | SatisfyingSetT
              | ListT
              | ExecutionT

