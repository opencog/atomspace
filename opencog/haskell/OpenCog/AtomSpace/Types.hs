-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes,
    DataKinds, ConstraintKinds, KindSignatures, StandaloneDeriving #-}

-- | This Module defines the main data types for Haskell bindings.
module OpenCog.AtomSpace.Types (
    TruthVal (..)
  , AtomName (..)
  , Atom (..)
  , AtomGen (..)
  , appAtomGen
  , showConstr
  ) where

import OpenCog.AtomSpace.Inheritance    (Is,AtomType(..))

-- | Atom name type.
type AtomName = String

-- | 'TruthVal' represent the different types of TruthValues.
data TruthVal = SimpleTV { tvMean       :: Double
                         , tvConfidence :: Double
                         }
              | CountTV  { tvMean       :: Double
                         , tvCount      :: Double
                         , tvConfidence :: Double
                         }
              | IndefTV  { tvMean       :: Double
                         , tvL          :: Double
                         , tvU          :: Double
                         , tvConfLevel  :: Double
                         , tvDiff       :: Double
                         }
              | FuzzyTV  { tvMean       :: Double
                         , tvConfidence :: Double
                         }
              | ProbTV   { tvMean       :: Double
                         , tvCount      :: Double
                         , tvConfidence :: Double
                         }
    deriving (Show)

-- | 'AtomGen' is a general atom type hiding the type variables.
-- (necessary when working with many instances of different atoms,
-- for example, for lists of atoms)
data AtomGen where
    AtomGen :: Atom a -> AtomGen

deriving instance Show AtomGen

-- | 'appAtomGen' evaluates a given function with the atom instance
-- wrapped inside the 'AtomGen' type.
appAtomGen :: (forall a. Atom a -> b) -> AtomGen -> b
appAtomGen f (AtomGen at) = f at

-- | 'Atom' is the main data type to represent the different types of atoms.
data Atom (a :: AtomType) where
    PredicateNode   :: AtomName -> Atom PredicateT
    AndLink         :: Is a ConceptT =>
                       Atom a -> Atom a -> (Maybe TruthVal) -> Atom AndT
    OrLink          :: Is a ConceptT =>
                       Atom a -> Atom a -> (Maybe TruthVal) -> Atom OrT
    ImplicationLink :: Atom a -> Atom b -> (Maybe TruthVal) -> Atom ImplicationT
    EquivalenceLink :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom EquivalenceT
    EvaluationLink  :: (Is p PredicateT,Is l ListT) =>
                       Atom p -> Atom l -> (Maybe TruthVal) -> Atom EvaluationT

    ConceptNode     :: AtomName -> (Maybe TruthVal) -> Atom ConceptT
    InheritanceLink :: (Is c1 ConceptT,Is c2 ConceptT) =>
                       Atom c1 -> Atom c2 -> (Maybe TruthVal) -> Atom InheritanceT
    SimilarityLink  :: (Is c1 ConceptT,Is c2 ConceptT) =>
                       Atom c1 -> Atom c2 -> (Maybe TruthVal) -> Atom SimilarityT
    MemberLink      :: (Is c1 ConceptT,Is c2 ConceptT) =>
                       Atom c1 -> Atom c2 -> (Maybe TruthVal) -> Atom MemberT
    SatisfyingSetLink :: (Is p PredicateT) =>
                       Atom p -> Atom SatisfyingSetT

    NumberNode :: Double -> Atom NumberT

    ListLink :: [AtomGen] -> Atom ListT

    SchemaNode :: AtomName -> Atom SchemaT
    GroundedSchemaNode :: AtomName -> Atom GroundedSchemaT
    ExecutionLink :: (Is s SchemaT,Is l ListT,Is a AtomT) =>
                     Atom s -> Atom l -> Atom a -> Atom ExecutionT

showConstr :: Atom a -> String
showConstr at = case at of
    PredicateNode _       -> "PredicateNode"
    AndLink _ _ _         -> "AndLink"
    OrLink _ _ _          -> "OrLink"
    ImplicationLink _ _ _ -> "ImplicationLink"
    EquivalenceLink _ _ _ -> "EquivalenceLink"
    EvaluationLink _ _ _  -> "EvaluationLink"
    ConceptNode _ _       -> "ConceptNode"
    InheritanceLink _ _ _ -> "InheritanceLink"
    SimilarityLink _ _ _  -> "SimilarityLink"
    MemberLink _ _ _      -> "MemberLink"
    SatisfyingSetLink _   -> "SatisfyingSetLink"
    NumberNode _          -> "NumberNode"
    ListLink _            -> "ListLink"
    SchemaNode _          -> "SchemaNode"
    GroundedSchemaNode _  -> "GroundedSchemaNode"
    ExecutionLink _ _ _   -> "ExecutionLink"

deriving instance Show (Atom a)

