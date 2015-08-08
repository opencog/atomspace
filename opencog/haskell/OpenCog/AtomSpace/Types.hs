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
  , Gen (..)
  , appGen
  , getType
  ) where

import OpenCog.AtomSpace.Inheritance    (type (<~))
import OpenCog.AtomSpace.AtomType       (AtomType(..))
import Data.Typeable                    (Typeable,typeRep)
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

-- | 'Gen' groups all the atoms that are children of the atom type a.
data Gen a where
    Gen :: (Typeable a,b <~ a) => Atom b -> Gen a

deriving instance Show (Gen a)

-- | 'appGen' evaluates a given function with the atom type instance
-- wrapped inside the 'Gen' type.
appGen :: (forall b. (Typeable a,b <~ a) => Atom b -> c) -> Gen a -> c
appGen f (Gen at) = f at

-- | 'AtomGen' is a general atom type hiding the type variables.
-- (necessary when working with many instances of different atoms,
-- for example, for lists of general atoms)
type AtomGen = Gen AtomT

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

getType :: (Typeable a) => Atom a -> AtomType
getType = read . show . typeRep
