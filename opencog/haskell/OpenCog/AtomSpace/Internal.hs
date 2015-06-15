{-# LANGUAGE GADTs #-}

module OpenCog.AtomSpace.Internal (
      Handle(..)
    , AtomType(..)
    , AtomRaw(..)
    , toRaw
    , fromRaw
    , TVTypeEnum(..)
    , tvMAX_PARAMS
    ) where

import Foreign.C.Types              (CULong(..))
import Data.Functor                 ((<$>))
import OpenCog.AtomSpace.Types      (Atom(..),AtomName(..),TruthVal(..),
                                     appAtomGen,AtomGen(..))

type Handle = CULong
type AtomType = String
data AtomRaw = Link AtomType [AtomRaw] (Maybe TruthVal)
             | Node AtomType AtomName  (Maybe TruthVal)

toRaw :: Atom a -> AtomRaw
toRaw i = case i of
    Predicate n  -> Node "PredicateNode" n Nothing
    And a1 a2 tv -> Link "AndLink" [toRaw a1,toRaw a2] tv
    Concept n    -> Node "ConceptNode" n Nothing
    List list    -> Link "ListLink" (map (appAtomGen toRaw) list) Nothing
    _            -> undefined

fromRaw :: AtomRaw -> Atom a -> Maybe (Atom a)
fromRaw raw orig = case (raw,orig) of
    (Node "ConceptNode" n _    , Concept _   ) -> Just $ Concept n
    (Node "PredicateNode" n _  , Predicate _ ) -> Just $ Predicate n
    (Link "AndLink" [ar,br] tv , And ao bo _ ) -> do
        a <- fromRaw ar ao
        b <- fromRaw br bo
        Just $ And a b tv
    (Link "ListLink" lraw _    , List lorig  ) -> do
        lnew <- if length lraw == length lorig
                 then sequence $ zipWith (\raw orig -> 
                                    appAtomGen
                                    ((<$>) AtomGen . fromRaw raw) orig)
                                    lraw lorig
                 else Nothing
        Just $ List lnew
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
