-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs, EmptyDataDecls, ExistentialQuantification, RankNTypes,
    DeriveDataTypeable, DataKinds, KindSignatures, StandaloneDeriving #-}

-- | This Module defines the main data types for Haskell bindings.
module OpenCog.AtomSpace.Types (
    TruthVal (..)
  , AtomName (..)
  , Atom (..)
  , AtomGen (..)
  , appAtomGen
  , showConstr
  ) where

import OpenCog.AtomSpace.Inheritance
import Data.Data                       (Typeable,Data)

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
    deriving (Show,Typeable,Data)

-- | 'AtomGen' is a general atom type hiding the type variables.
-- (necessary when working with many instances of different atoms,
-- for example, for lists of atoms)
data AtomGen where
    AtomGen :: Atom a -> AtomGen

deriving instance Typeable AtomGen
deriving instance Show AtomGen

-- | 'appAtomGen' evaluates a given function with the atom instance
-- wrapped inside the 'AtomGen' type.
appAtomGen :: (forall a. Atom a -> b) -> AtomGen -> b
appAtomGen f (AtomGen at) = f at

-- | 'Atom' is the main data type to represent the different types of atoms.
data Atom (a :: AtomType) where
    PredicateNode   :: AtomName -> Atom PredicateNodeT
    AndLink         :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom AndLinkT
    OrLink          :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom OrLinkT
    ImplicationLink :: Atom a -> Atom b -> (Maybe TruthVal) -> Atom ImplicationLinkT
    EquivalenceLink :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom EquivalenceLinkT
    EvaluationLink  :: (IsPredicate p,IsList l) =>
                       Atom p -> Atom l -> (Maybe TruthVal) -> Atom EvaluationLinkT

    ConceptNode     :: AtomName -> (Maybe TruthVal) -> Atom ConceptNodeT
    InheritanceLink :: (IsConcept c1,IsConcept c2) =>
                       Atom c1 -> Atom c2 -> (Maybe TruthVal) -> Atom InheritanceLinkT
    SimilarityLink  :: (IsConcept c1,IsConcept c2) =>
                       Atom c1 -> Atom c2 -> (Maybe TruthVal) -> Atom SimilarityLinkT
    MemberLink      :: (IsConcept c1,IsConcept c2) =>
                       Atom c1 -> Atom c2 -> (Maybe TruthVal) -> Atom MemberLinkT
    SatisfyingSetLink :: (IsPredicate p) =>
                       Atom p -> Atom SatisfyingSetLinkT

    NumberNode :: Double -> Atom NumberNodeT

    ListLink :: [AtomGen] -> Atom ListLinkT

    SchemaNode :: AtomName -> Atom SchemaNodeT
    GroundedSchemaNode :: AtomName -> Atom GroundedSchemaNodeT
    ExecutionLink :: (IsSchema s,IsList l,IsAtom a) =>
                     Atom s -> Atom l -> Atom a -> Atom ExecutionLinkT

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
deriving instance Typeable Atom

