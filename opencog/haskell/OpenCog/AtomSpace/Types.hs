-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE AutoDeriveTypeable        #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE StandaloneDeriving        #-}

-- | This Module defines the main data types for Haskell bindings.
module OpenCog.AtomSpace.Types (
    TruthVal (..)
  , AtomName (..)
  , Atom (..)
  , AtomGen (..)
  , appAtomGen
  , getType
  , noTv
  , withTv
  ) where

import OpenCog.AtomSpace.Inheritance    (type (<~))
import OpenCog.AtomSpace.AtomType       (AtomType(..))
import Data.Typeable                    (Typeable)
import Control.Monad                    (Functor,Monad)

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
    deriving (Show,Eq)

type TVal = Maybe TruthVal

noTv = Nothing
withTv = Just

-- | 'AtomGen' is a general atom type hiding the type variables.
-- (necessary when working with many instances of different atoms,
-- for example, for lists of atoms)
data AtomGen where
    AtomGen :: (a <~ AtomT) => Atom a -> AtomGen

deriving instance Show AtomGen

-- | 'appAtomGen' evaluates a given function with the atom instance
-- wrapped inside the 'AtomGen' type.
appAtomGen :: (forall a. (a <~ AtomT) => Atom a -> b) -> AtomGen -> b
appAtomGen f (AtomGen at) = f at

-- | 'Atom' is the main data type to represent the different types of atoms.
data Atom (a :: AtomType) where
    PredicateNode       :: AtomName -> TVal -> Atom PredicateT

    AndLink             :: (a <~ AtomT,b <~ AtomT) =>
                           TVal -> Atom a -> Atom b -> Atom AndT
    OrLink              :: (a <~ AtomT,b <~ AtomT) =>
                           TVal -> Atom a -> Atom b -> Atom OrT
    ImplicationLink     :: (a <~ AtomT,b <~ AtomT) =>
                           TVal -> Atom a -> Atom b -> Atom ImplicationT
    EquivalenceLink     :: (a <~ AtomT,b <~ AtomT) =>
                           TVal -> Atom a -> Atom b -> Atom EquivalenceT
    EvaluationLink      :: (p <~ PredicateT,l <~ ListT) =>
                           TVal -> Atom p -> Atom l -> Atom EvaluationT

    ConceptNode         :: AtomName -> TVal -> Atom ConceptT

    InheritanceLink     :: (c1 <~ ConceptT,c2 <~ ConceptT) =>
                           TVal -> Atom c1 -> Atom c2 -> Atom InheritanceT
    SimilarityLink      :: (c1 <~ ConceptT,c2 <~ ConceptT) =>
                           TVal -> Atom c1 -> Atom c2 -> Atom SimilarityT
    MemberLink          :: (c1 <~ NodeT,c2 <~ NodeT) =>
                           TVal -> Atom c1 -> Atom c2 -> Atom MemberT
    SatisfyingSetLink   :: (p <~ PredicateT) =>
                           Atom p -> Atom SatisfyingSetT

    NumberNode          :: Double -> Atom NumberT

    ListLink            :: [AtomGen] -> Atom ListT

    SchemaNode          :: AtomName -> Atom SchemaT
    GroundedSchemaNode  :: AtomName -> Atom GroundedSchemaT
    ExecutionLink       :: (s <~ SchemaT,l <~ ListT,a <~ AtomT) =>
                           Atom s -> Atom l -> Atom a -> Atom ExecutionT

    VariableNode        :: AtomName -> Atom VariableT

    SatisfactionLink    :: (v <~ VariableT,l <~ LinkT) =>
                           Atom v -> Atom l -> Atom SatisfactionT
    ForAllLink          :: (v <~ ListT,i <~ ImplicationT) =>
                           TVal -> Atom v -> Atom i -> Atom ForAllT

    AverageLink         :: (v <~ VariableT,a <~ AtomT) =>
                           TVal -> Atom v -> Atom a -> Atom AverageT


deriving instance Show (Atom a)
deriving instance Typeable Atom

getType :: Atom a -> AtomType
getType at = case at of
    PredicateNode _ _     -> PredicateT
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
    VariableNode _        -> VariableT
    SatisfactionLink _ _  -> SatisfactionT
    ForAllLink _ _ _      -> ForAllT
    AverageLink _ _ _     -> AverageT

