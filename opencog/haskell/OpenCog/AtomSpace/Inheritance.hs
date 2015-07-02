-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE EmptyDataDecls #-}

-- | This Module defines the relation between different atom types.
module OpenCog.AtomSpace.Inheritance where

data AtomT
data NodeT
data LinkT
data PredicateNodeT
data ConceptNodeT
data SchemaNodeT
data GroundedSchemaNodeT
data NumberNodeT
data AndLinkT
data OrLinkT
data ImplicationLinkT
data EquivalenceLinkT
data EvaluationLinkT
data InheritanceLinkT
data SimilarityLinkT
data MemberLinkT
data SatisfyingSetLinkT
data ListLinkT
data ExecutionLinkT

class IsAtom a
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

instance IsAtom AndLinkT
instance IsLink AndLinkT
instance IsAnd AndLinkT
instance IsAtom OrLinkT
instance IsLink OrLinkT
instance IsOr OrLinkT
instance IsAtom PredicateNodeT
instance IsNode PredicateNodeT
instance IsPredicate PredicateNodeT
instance IsAtom ImplicationLinkT
instance IsLink ImplicationLinkT
instance IsImplication ImplicationLinkT
instance IsAtom EquivalenceLinkT
instance IsLink EquivalenceLinkT
instance IsEquivalence EquivalenceLinkT
instance IsAtom EvaluationLinkT
instance IsLink EvaluationLinkT
instance IsEvaluation EvaluationLinkT
instance IsAtom ConceptNodeT
instance IsNode ConceptNodeT
instance IsConcept ConceptNodeT
instance IsAtom InheritanceLinkT
instance IsLink InheritanceLinkT
instance IsInheritance InheritanceLinkT
instance IsAtom SimilarityLinkT
instance IsLink SimilarityLinkT
instance IsSimilarity SimilarityLinkT
instance IsAtom MemberLinkT
instance IsLink MemberLinkT
instance IsMember MemberLinkT
instance IsAtom SatisfyingSetLinkT
instance IsLink SatisfyingSetLinkT
instance IsSatisfyingSet SatisfyingSetLinkT
instance IsAtom NumberNodeT
instance IsNode NumberNodeT
instance IsNumber NumberNodeT
instance IsAtom ListLinkT
instance IsLink ListLinkT
instance IsList ListLinkT
instance IsAtom SchemaNodeT
instance IsNode SchemaNodeT
instance IsSchema SchemaNodeT
instance IsAtom GroundedSchemaNodeT
instance IsNode GroundedSchemaNodeT
instance IsSchema GroundedSchemaNodeT
instance IsGroundedSchema GroundedSchemaNodeT
instance IsAtom ExecutionLinkT
instance IsLink ExecutionLinkT
instance IsExecution ExecutionLinkT

