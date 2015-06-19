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

data Atom a where
    PredicateNode   :: AtomName -> Atom (Atom a -> TruthVal)
    AndLink         :: Atom a -> Atom b -> (Maybe TruthVal) -> Atom c
    OrLink          :: Atom a -> Atom b -> (Maybe TruthVal) -> Atom c
    ImplicationLink :: Atom a -> Atom b -> (Maybe TruthVal) -> Atom c
    EquivalenceLink :: Atom a -> Atom b -> (Maybe TruthVal) -> Atom c
    EvaluationLink  :: (Atom (Atom a -> TruthVal))  ->
                      Atom [AtomGen] -> (Maybe TruthVal) -> Atom b

    ConceptNode       :: AtomName -> (Maybe TruthVal) -> Atom TConceptNode
    InheritanceLink   :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    SimilarityLink    :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    MemberLink        :: Atom TConceptNode -> Atom TConceptNode -> (Maybe TruthVal) -> Atom a
    SatisfyingSetLink :: Atom (Atom a -> TruthVal) -> Atom TConceptNode

    NumberNode :: (Num a,Show a) => a -> Atom TNumberNode

    ListLink :: [AtomGen] -> Atom [AtomGen]


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

