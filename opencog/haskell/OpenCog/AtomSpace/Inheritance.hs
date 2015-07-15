-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE EmptyDataDecls, StandaloneDeriving, DeriveDataTypeable,
             FlexibleInstances, DataKinds, KindSignatures, TypeFamilies,
             TypeOperators, ConstraintKinds, UndecidableInstances #-}

-- | This Module defines the relation between different atom types.
module OpenCog.AtomSpace.Inheritance (
    AtomType(..)
  , Is
  , fromAtomTypeRaw
  , toAtomTypeRaw
  ) where

import GHC.Exts     (Constraint)

-- | 'Up' given an atom type returns a list with its parent atom types.
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

-- | 'AtomType' kind groups all atom types.
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

toAtomTypeRaw :: AtomType -> String
toAtomTypeRaw at = case at of
    PredicateT      -> "PredicateNode"
    AndT            -> "AndLink"
    OrT             -> "OrLink"
    ImplicationT    -> "ImplicationLink"
    EquivalenceT    -> "EquivalenceLink"
    EvaluationT     -> "EvaluationLink"
    ConceptT        -> "ConceptNode"
    InheritanceT    -> "InheritanceLink"
    SimilarityT     -> "SimilarityLink"
    MemberT         -> "MemberLink"
    SatisfyingSetT  -> "SatisfyingSetLink"
    NumberT         -> "NumberNode"
    ListT           -> "ListLink"
    SchemaT         -> "SchemaNode"
    GroundedSchemaT -> "GroundedSchemaNode"
    ExecutionT      -> "ExecutionLink"


fromAtomTypeRaw :: String -> Maybe AtomType
fromAtomTypeRaw s = case s of
    "PredicateNode"      -> Just PredicateT
    "AndLink"            -> Just AndT
    "OrLink"             -> Just OrT
    "ImplicationLink"    -> Just ImplicationT
    "EquivalenceLink"    -> Just EquivalenceT
    "EvaluationLink"     -> Just EvaluationT
    "ConceptNode"        -> Just ConceptT
    "InheritanceLink"    -> Just InheritanceT
    "SimilarityLink"     -> Just SimilarityT
    "MemberLink"         -> Just MemberT
    "SatisfyingSetLink"  -> Just SatisfyingSetT
    "NumberNode"         -> Just NumberT
    "ListLink"           -> Just ListT
    "SchemaNode"         -> Just SchemaT
    "GroundedSchemaNode" -> Just GroundedSchemaT
    "ExecutionLink"      -> Just ExecutionT
    _                    -> Nothing
