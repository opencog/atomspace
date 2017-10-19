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
  , AtomType (..)
  , Atom(..)
  ) where

-- | Atom name type.
type AtomName = String
type AtomType = String

-- Main general atom representation.
data Atom = Link AtomType [Atom] TruthVal
          | Node AtomType AtomName TruthVal
          deriving (Eq,Show,Read)

data Value = FloatValue     AtomType    [Double]
           | StringValue    AtomType    String
           | LinkValue      AtomType    [Value]

-- | 'TruthVal' represent the different types of TruthValues.
data TruthVal = SimpleTV { tvMean       :: Double
                         , tvConfidence :: Double
                         }
              | CountTV  { tvMean       :: Double
                         , tvConfidence :: Double
                         , tvCount      :: Double
                         }
              | IndefTV  { tvMean       :: Double
                         , tvU          :: Double
                         , tvL          :: Double
                         , tvConfLevel  :: Double
                         , tvDiff       :: Double
                         }
              | FuzzyTV  { tvMean       :: Double
                         , tvCount      :: Double
                         }
              | ProbTV   { tvMean       :: Double
                         , tvConfidence :: Double
                         , tvCount      :: Double
                         }
    deriving (Show,Read,Eq)
