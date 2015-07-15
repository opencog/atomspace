-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes, AutoDeriveTypeable,
    DataKinds, ConstraintKinds, KindSignatures, StandaloneDeriving #-}

-- | This Module defines the main data types for Haskell bindings.
module OpenCog.AtomSpace.Types (
    TruthVal (..)
  , AtomName (..)
  , Atom (..)
  , AtomGen (..)
  , appAtomGen
  , getType
  ) where

import OpenCog.AtomSpace.Inheritance    (Is)
import OpenCog.AtomSpace.AtomType       (AtomType(..))
import Data.Typeable                    (Typeable)

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
    AtomGen :: Is a AtomT => Atom a -> AtomGen

deriving instance Show AtomGen

-- | 'appAtomGen' evaluates a given function with the atom instance
-- wrapped inside the 'AtomGen' type.
appAtomGen :: (forall a. Is a AtomT => Atom a -> b) -> AtomGen -> b
appAtomGen f (AtomGen at) = f at

-- | 'Atom' is the main data type to represent the different types of atoms.
data Atom (a :: AtomType) where
    PredicateNode   :: AtomName -> Atom PredicateT
    AndLink         :: (Is a ConceptT,Is b ConceptT) =>
                       Atom a -> Atom b -> (Maybe TruthVal) -> Atom AndT
    OrLink          :: (Is a ConceptT,Is b ConceptT) =>
                       Atom a -> Atom b -> (Maybe TruthVal) -> Atom OrT
    ImplicationLink :: (Is a AtomT,Is b AtomT) =>
                       Atom a -> Atom b -> (Maybe TruthVal) -> Atom ImplicationT
    EquivalenceLink :: (Is a AtomT,Is b AtomT) =>
                       Atom a -> Atom b -> (Maybe TruthVal) -> Atom EquivalenceT
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

deriving instance Show (Atom a)
deriving instance Typeable Atom

getType :: Atom a -> AtomType
getType at = case at of
    PredicateNode _       -> PredicateT
    AndLink _ _ _         -> AndT
    OrLink _ _ _          -> OrT
    ImplicationLink _ _ _ -> ImplicationT
    EquivalenceLink _ _ _ -> EquivalenceT
    EvaluationLink _ _ _  -> EvaluationT
    ConceptNode _ _       -> ConceptT
    InheritanceLink _ _ _ -> InheritanceT
    SimilarityLink _ _ _  -> SimilarityT
    MemberLink _ _ _      -> MemberT
    SatisfyingSetLink _   -> SatisfyingSetT
    NumberNode _          -> NumberT
    ListLink _            -> ListT
    SchemaNode _          -> SchemaT
    GroundedSchemaNode _  -> GroundedSchemaT
    ExecutionLink _ _ _   -> ExecutionT

