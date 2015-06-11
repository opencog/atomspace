{-# LANGUAGE GADTs , EmptyDataDecls , ExistentialQuantification , RankNTypes #-}

module OpenCog.AtomSpace.Types (
    TruthVal (..)
  , AtomName (..)
  , Atom (..)
  , AtomGen (..)
  , appAtomGen
  , TNumberNode
  , TConceptNode
  ) where

type AtomName = String

data TruthVal = SimpleTruthVal Double Double
              | CountTruthVal Double Double Double
              | IndefiniteTruthVal Double Double Double Double
    deriving Show

data TConceptNode
data TNumberNode

data AtomGen where
    AtomGen :: Atom a -> AtomGen

appAtomGen :: (forall a. Atom a -> b) -> AtomGen -> b
appAtomGen f (AtomGen at) = f at

data Atom a where -- TODO: Review the types constraints, add Attention Values, etc.
    -- Predicate
    Predicate   :: AtomName -> Atom (Atom a -> TruthVal)
    And         :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    Or          :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    Implication :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    Equivalence :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    Evaluation  :: (Atom (Atom a -> TruthVal))  ->
                      Atom [AtomGen] -> (Maybe TruthVal) -> Atom a

    -- Concept
    Concept       :: AtomName -> Atom TConceptNode
    Inheritance   :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    Similarity    :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    Member        :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    SatisfyingSet :: Atom (Atom a -> TruthVal) -> Atom TConceptNode

    -- Number
    Number :: (Num a) => a -> Atom TNumberNode

    -- List
    List :: [AtomGen] -> Atom [AtomGen]

