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

data TConceptNode
data TNumberNode

data AtomGen where
    AtomGen :: Atom a -> AtomGen

appAtomGen :: (forall a. Atom a -> b) -> AtomGen -> b
appAtomGen f (AtomGen at) = f at

data Atom a where -- TODO: Review the types constraints, add Attention Values, etc.
    -- Predicate
    PredicateNode   :: AtomName -> Atom (Atom a -> TruthVal)
    AndLink         :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    OrLink          :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    ImplicationLink :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    EquivalenceLink :: Atom a -> Atom a -> (Maybe TruthVal) -> Atom a
    EvaluationLink  :: (Atom (Atom a -> TruthVal))  ->
                      Atom [AtomGen] -> (Maybe TruthVal) -> Atom a

    -- Concept
    ConceptNode       :: AtomName -> (Maybe TruthVal) -> Atom TConceptNode
    InheritanceLink   :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    SimilarityLink    :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    MemberLink        :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    SatisfyingSetLink :: Atom (Atom a -> TruthVal) -> Atom TConceptNode

    -- Number
    NumberNode :: (Num a,Show a) => a -> Atom TNumberNode

    -- List
    ListLink :: [AtomGen] -> Atom [AtomGen]

instance Show AtomGen where
    show (AtomGen at) = concat' ["AtomGen",show at]

instance Show (Atom a) where
    show (PredicateNode n)         = concat' ["Predicate",show n]
    show (AndLink a1 a2 m)         = concat' ["And",show a1,show a2,show m]
    show (OrLink a1 a2 m)          = concat' ["Or",show a1,show a2,show m]
    show (ImplicationLink a1 a2 m) = concat' ["Implication",show a1,show a2,show m]
    show (EquivalenceLink a1 a2 m) = concat' ["Equivalence",show a1,show a2,show m]
    show (EvaluationLink a1 a2 m)  = concat' ["Evaluation",show a1,show a2,show m]
    show (ConceptNode n m)         = concat' ["Concept",show n,show m]
    show (InheritanceLink a1 a2 m) = concat' ["Inheritance",show a1,show a2,show m]
    show (SimilarityLink a1 a2 m)  = concat' ["Similarity",show a1,show a2,show m]
    show (MemberLink a1 a2 m)      = concat' ["Member",show a1,show a2,show m]
    show (SatisfyingSetLink a)     = concat' ["SatisfyingSet",show a]
    show (NumberNode n)            = concat' ["Number",show n]
    show (ListLink l)              = concat' ["List",show l]

concat' (a:b:xs) = a ++ " " ++ concat' (b:xs)
concat' (b:[])   = b
concat' []       = ""
