-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs , EmptyDataDecls , ExistentialQuantification , RankNTypes #-}

-- | This Module defines the main data types for Haskell bindings.
module OpenCog.AtomSpace.Types (
    TruthVal (..)
  , AtomName (..)
  , Atom (..)
  , AtomGen (..)
  , appAtomGen
  ) where

import OpenCog.AtomSpace.Inheritance

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
              | IndefTV  { tvMean      :: Double
                         , tvL         :: Double
                         , tvU         :: Double
                         , tvConfLevel :: Double
                         , tvDiff      :: Double
                         }
              | FuzzyTV  { tvMean       :: Double
                         , tvConfidence :: Double
                         }
              | ProbTV   { tvMean       :: Double
                         , tvCount      :: Double
                         , tvConfidence :: Double
                         }
    deriving Show

-- | 'AtomGen' is a general atom type hiding the type variables.
-- (necessary when working with many instances of different atoms,
-- for example, for lists of atoms)
data AtomGen where
    AtomGen :: Atom a -> AtomGen

-- | 'appAtomGen' evaluates a given function with the atom instance
-- wrapped inside the 'AtomGen' type.
appAtomGen :: (forall a. Atom a -> b) -> AtomGen -> b
appAtomGen f (AtomGen at) = f at

-- | 'Atom' is the main data type to represent the different types of atoms.
data Atom a where
    PredicateNode   :: AtomName -> Atom PredicateNodeT
    AndLink         :: IsAtom a =>
                       Atom a -> Atom a -> (Maybe TruthVal) -> Atom AndLinkT
    OrLink          :: IsAtom a =>
                       Atom a -> Atom a -> (Maybe TruthVal) -> Atom OrLinkT
    ImplicationLink :: (IsAtom a,IsAtom b) =>
                       Atom a -> Atom b -> (Maybe TruthVal) -> Atom ImplicationLinkT
    EquivalenceLink :: IsAtom a =>
                       Atom a -> Atom a -> (Maybe TruthVal) -> Atom EquivalenceLinkT
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

    NumberNode :: (Show a,Num a) => a -> Atom NumberNodeT

    ListLink :: [AtomGen] -> Atom ListLinkT

    SchemaNode :: AtomName -> Atom SchemaNodeT
    GroundedSchemaNode :: AtomName -> Atom GroundedSchemaNodeT
    ExecutionLink :: (IsSchema s,IsList l,IsAtom a) =>
                     Atom s -> Atom l -> Atom a -> Atom ExecutionLinkT


instance Show AtomGen where
    show (AtomGen at) = concatWSpaces ["AtomGen",show at]

instance Show (Atom a) where
    show (PredicateNode n)         = mix ["PredicateNode",show n]
    show (AndLink a1 a2 m)         = mix ["AndLink",show a1,show a2,show m]
    show (OrLink a1 a2 m)          = mix ["OrLink",show a1,show a2,show m]
    show (ImplicationLink a1 a2 m) = mix ["ImplicationLink",show a1,show a2,show m]
    show (EquivalenceLink a1 a2 m) = mix ["EquivalenceLink",show a1,show a2,show m]
    show (EvaluationLink a1 a2 m)  = mix ["EvaluationLink",show a1,show a2,show m]
    show (ConceptNode n m)         = mix ["ConceptNode",show n,show m]
    show (InheritanceLink a1 a2 m) = mix ["InheritanceLink",show a1,show a2,show m]
    show (SimilarityLink a1 a2 m)  = mix ["SimilarityLink",show a1,show a2,show m]
    show (MemberLink a1 a2 m)      = mix ["MemberLink",show a1,show a2,show m]
    show (SatisfyingSetLink a)     = mix ["SatisfyingSetLink",show a]
    show (NumberNode n)            = mix ["NumberNode",show n]
    show (ListLink l)              = mix ["ListLink",show l]

mix :: [String] -> String
mix xs = "( "++concatWSpaces xs++")"

concatWSpaces :: [String] -> String
concatWSpaces = foldr (\a b -> a++" "++b) []

