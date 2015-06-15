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

data TruthVal = SimpleTV { tvMean       :: Double
                         , tvConfidence :: Double
                         }
              | CountTV { tvMean       :: Double
                        , tvCount      :: Double
                        , tvConfidence :: Double
                        }
              | IndefTV { tvMean      :: Double
                        , tvL         :: Double
                        , tvU         :: Double
                        , tvConfLevel :: Double
                        , tvDiff      :: Double
                        }
              | FuzzyTV { tvMean       :: Double
                        , tvConfidence :: Double
                        }
              | ProbTV { tvMean       :: Double
                       , tvCount      :: Double
                       , tvConfidence :: Double
                       }
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
    Number :: (Num a,Show a) => a -> Atom TNumberNode

    -- List
    List :: [AtomGen] -> Atom [AtomGen]

instance Show AtomGen where
    show (AtomGen at) = concat' ["AtomGen",show at]

instance Show (Atom a) where
    show (Predicate n)         = concat' ["Predicate",show n]
    show (And a1 a2 m)         = concat' ["And",show a1,show a2,show m]
    show (Or a1 a2 m)          = concat' ["Or",show a1,show a2,show m]
    show (Implication a1 a2 m) = concat' ["Implication",show a1,show a2,show m]
    show (Equivalence a1 a2 m) = concat' ["Equivalence",show a1,show a2,show m]
    show (Evaluation a1 a2 m)  = concat' ["Evaluation",show a1,show a2,show m]
    show (Concept n)           = concat' ["Concept",show n]
    show (Inheritance a1 a2 m) = concat' ["Inheritance",show a1,show a2,show m]
    show (Similarity a1 a2 m)  = concat' ["Similarity",show a1,show a2,show m]
    show (Member a1 a2 m)      = concat' ["Member",show a1,show a2,show m]
    show (SatisfyingSet a)     = concat' ["SatisfyingSet",show a]
    show (Number n)            = concat' ["Number",show n]
    show (List l)              = concat' ["List",show l]

concat' (a:b:xs) = a ++ " " ++ concat' (b:xs)
concat' (b:[])   = b
concat' []       = ""
