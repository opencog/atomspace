-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE EmptyDataDecls, StandaloneDeriving, DeriveDataTypeable,
             FlexibleInstances, DataKinds, KindSignatures #-}
-- | This Module defines the relation between different atom types.
module OpenCog.AtomSpace.Inheritance where

import Data.Data    (Typeable,Data)

data AtomType = AtomT
              | NodeT
              | LinkT
              | PredicateNodeT
              | ConceptNodeT
              | SchemaNodeT
              | GroundedSchemaNodeT
              | NumberNodeT
              | AndLinkT
              | OrLinkT
              | ImplicationLinkT
              | EquivalenceLinkT
              | EvaluationLinkT
              | InheritanceLinkT
              | SimilarityLinkT
              | MemberLinkT
              | SatisfyingSetLinkT
              | ListLinkT
              | ExecutionLinkT

deriving instance Typeable AtomType
deriving instance Data AtomType

class IsAtom (a :: AtomType)
class IsAtom a => IsNode a
class IsAtom a => IsLink a
class IsLink a => IsAnd a
class IsLink a => IsOr a
class IsLink a => IsImplication a
class IsLink a => IsEquivalence a
class IsLink a => IsEvaluation a
class IsLink a => IsInheritance a
class IsLink a => IsSimilarity a
class IsLink a => IsMember a
class IsLink a => IsSatisfyingSet a
class IsLink a => IsList a
class IsLink a => IsExecution a
class IsNode a => IsPredicate a
class IsNode a => IsNumber a
class IsNode a => IsConcept a
class IsNode a => IsSchema a
class IsSchema a => IsGroundedSchema a

instance IsAtom a

instance IsLink AndLinkT
instance IsAnd AndLinkT

instance IsLink OrLinkT
instance IsOr OrLinkT

instance IsNode PredicateNodeT
instance IsPredicate PredicateNodeT

instance IsLink ImplicationLinkT
instance IsImplication ImplicationLinkT

instance IsLink EquivalenceLinkT
instance IsEquivalence EquivalenceLinkT

instance IsLink EvaluationLinkT
instance IsEvaluation EvaluationLinkT

instance IsNode ConceptNodeT
instance IsConcept ConceptNodeT

instance IsLink InheritanceLinkT
instance IsInheritance InheritanceLinkT

instance IsLink SimilarityLinkT
instance IsSimilarity SimilarityLinkT

instance IsLink MemberLinkT
instance IsMember MemberLinkT

instance IsLink SatisfyingSetLinkT
instance IsSatisfyingSet SatisfyingSetLinkT

instance IsNode NumberNodeT
instance IsNumber NumberNodeT

instance IsLink ListLinkT
instance IsList ListLinkT

instance IsNode SchemaNodeT
instance IsSchema SchemaNodeT

instance IsNode GroundedSchemaNodeT
instance IsSchema GroundedSchemaNodeT
instance IsGroundedSchema GroundedSchemaNodeT

instance IsLink ExecutionLinkT
instance IsExecution ExecutionLinkT

