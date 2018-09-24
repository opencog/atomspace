-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This Module defines some useful data types for proper interaction
-- with the AtomSpace C wrapper library.
-- Intended for internal use only.
module OpenCog.AtomSpace.Internal (
      Handle(..)
    , HandleSeq(..)
    , TruthValueP(..)
    , TVRaw(..)
    , fromTVRaw
    , toTVRaw
    , tvMAX_PARAMS
    ) where

import Foreign                       (Ptr)
import Data.Functor                  ((<$>))
import Data.Typeable                 (cast,Typeable)
import OpenCog.AtomSpace.Sugar       (noTv)
import OpenCog.AtomSpace.Types       (AtomName(..),AtomType(..)
                                     ,Atom(..),TruthVal(..))
type Handle = Ptr ()
type HandleSeq = Ptr Handle
type TruthValueP = Ptr ()
-- Constant with the maximum number of parameters in any type of TV.
tvMAX_PARAMS :: Int
tvMAX_PARAMS = 5

data TVRaw = TVRaw String [Double] deriving (Eq,Show)

toTVRaw :: TruthVal -> TVRaw
toTVRaw (SimpleTV a b     ) = TVRaw "SimpleTruthValue"        [a,b]
toTVRaw (CountTV a b c    ) = TVRaw "CountTruthValue"         [a,b,c]
toTVRaw (IndefTV a b c d e) = TVRaw "IndefiniteTruthValue"    [a,b,c,d,e]
toTVRaw (FuzzyTV a b      ) = TVRaw "FuzzyTruthValue"         [a,b]
toTVRaw (ProbTV a b c     ) = TVRaw "ProbabilisticTruthValue" [a,b,c]

fromTVRaw :: TVRaw -> TruthVal
fromTVRaw (TVRaw "SimpleTruthValue"       (a:b:_)      ) = SimpleTV a b
fromTVRaw (TVRaw "CountTruthValue"        (a:b:c:_)    ) = CountTV a b c
fromTVRaw (TVRaw "IndefiniteTruthValue"   (a:b:c:d:e:_)) = IndefTV a b c d e
fromTVRaw (TVRaw "FuzzyTruthValue"        (a:b:_)      ) = FuzzyTV a b
fromTVRaw (TVRaw "ProbabilisticTruthValue" (a:b:c:_)   ) = ProbTV a b c
fromTVRaw tv  = error $ "Don't know hot to handel TV of type: " ++ (show tv)

