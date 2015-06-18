{-# LANGUAGE GADTs #-}

module OpenCog.AtomSpace.Internal (
      Handle(..)
    , AtomType(..)
    , AtomRaw(..)
    , toRaw
    , fromRaw
    , TVRaw(..)
    , TVTypeEnum(..)
    , tvMAX_PARAMS
    ) where

import Foreign.C.Types              (CULong(..))
import Data.Functor                 ((<$>))
import OpenCog.AtomSpace.Types      (Atom(..),AtomName(..),TruthVal(..),
                                     appAtomGen,AtomGen(..))

type Handle = CULong
type AtomType = String
data AtomRaw = Link AtomType [AtomRaw] (Maybe TVRaw)
             | Node AtomType AtomName  (Maybe TVRaw)

toRaw :: Atom a -> AtomRaw
toRaw i = case i of
    PredicateNode n  -> Node "PredicateNode" n Nothing
    AndLink a1 a2 tv -> Link "AndLink" [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    ConceptNode n tv -> Node "ConceptNode" n $ toTVRaw <$> tv
    ListLink list    -> Link "ListLink" (map (appAtomGen toRaw) list) Nothing
    _            -> undefined

fromRaw :: AtomRaw -> Atom a -> Maybe (Atom a)
fromRaw raw orig = case (raw,orig) of
    (Node "ConceptNode" n tv   , ConceptNode _ _ ) -> Just $ ConceptNode n $ fromTVRaw <$> tv
    (Node "PredicateNode" n _  , PredicateNode _ ) -> Just $ PredicateNode n
    (Link "AndLink" [ar,br] tv , AndLink ao bo _ ) -> do
        a <- fromRaw ar ao
        b <- fromRaw br bo
        Just $ AndLink a b $ fromTVRaw <$> tv
    (Link "ListLink" lraw _    , ListLink lorig  ) -> do
        lnew <- if length lraw == length lorig
                 then sequence $ zipWith (\raw orig -> 
                                    appAtomGen
                                    ((<$>) AtomGen . fromRaw raw) orig)
                                    lraw lorig
                 else Nothing
        Just $ ListLink lnew
    _                                               -> Nothing -- undefined

--Constant with the maximum number of parameters in any type of TV.
tvMAX_PARAMS :: Int
tvMAX_PARAMS = 5

--TV enum type to work with TruthValueTypes from
-- <opencog/atomspace/TruthValue.h> definition.
data TVTypeEnum = NULL_TRUTH_VALUE
                | SIMPLE_TRUTH_VALUE
                | COUNT_TRUTH_VALUE
                | INDEFINITE_TRUTH_VALUE
                | FUZZY_TRUTH_VALUE
                | PROBABILISTIC_TRUTH_VALUE
    deriving Enum

data TVRaw = TVRaw TVTypeEnum [Double]

toTVRaw :: TruthVal -> TVRaw
toTVRaw (SimpleTV a b     ) = TVRaw SIMPLE_TRUTH_VALUE [a,b]
toTVRaw (CountTV a b c    ) = TVRaw COUNT_TRUTH_VALUE [a,b,c]
toTVRaw (IndefTV a b c d e) = TVRaw INDEFINITE_TRUTH_VALUE [a,b,c,d,e]
toTVRaw (FuzzyTV a b      ) = TVRaw FUZZY_TRUTH_VALUE [a,b]
toTVRaw (ProbTV a b c     ) = TVRaw PROBABILISTIC_TRUTH_VALUE [a,b,c]

fromTVRaw :: TVRaw -> TruthVal
fromTVRaw (TVRaw SIMPLE_TRUTH_VALUE (a:b:_))  = SimpleTV a b
fromTVRaw (TVRaw COUNT_TRUTH_VALUE (a:b:c:_)) = CountTV a b c
fromTVRaw (TVRaw INDEFINITE_TRUTH_VALUE (a:b:c:d:e:_)) = IndefTV a b c d e
fromTVRaw (TVRaw FUZZY_TRUTH_VALUE (a:b:_))   = FuzzyTV a b
fromTVRaw (TVRaw PROBABILISTIC_TRUTH_VALUE (a:b:c:_))  = ProbTV a b c

