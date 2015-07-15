-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs,DataKinds #-}

-- | This Module defines some useful data types for proper interaction
-- with the AtomSpace C wrapper library.
-- Intended for internal use only.
module OpenCog.AtomSpace.Internal (
      Handle(..)
    , AtomTypeRaw(..)
    , AtomRaw(..)
    , toRaw
    , fromRaw
    , TVRaw(..)
    , fromTVRaw
    , toTVRaw
    , TVTypeEnum(..)
    , tvMAX_PARAMS
    ) where

import Foreign.C.Types               (CULong(..))
import Data.Functor                  ((<$>))
import OpenCog.AtomSpace.Filter      (Gen(..),FilterIsChild(..))
import OpenCog.AtomSpace.AtomType    (AtomType(..),fromAtomTypeRaw,toAtomTypeRaw)
import OpenCog.AtomSpace.Types       (Atom(..),AtomName(..),getType,TruthVal(..),
                                      appAtomGen,AtomGen(..))

-- Data type to hold atoms's UUID.
type Handle = CULong
type AtomTypeRaw = String

-- Main general atom representation.
data AtomRaw = Link AtomTypeRaw [AtomRaw] (Maybe TVRaw)
             | Node AtomTypeRaw AtomName  (Maybe TVRaw)

-- Function to convert an Atom to its general representation.
toRaw :: Atom a -> AtomRaw
toRaw at = let atype = toAtomTypeRaw $ getType at
           in case at of
    PredicateNode n  -> Node atype n Nothing
    AndLink a1 a2 tv -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    ConceptNode n tv -> Node atype n $ toTVRaw <$> tv
    ListLink list    -> Link atype (map (appAtomGen toRaw) list) Nothing
    _                -> undefined

-- Function to get an Atom back from its general representation (if possible).
fromRaw :: AtomRaw -> Atom a -> Maybe (Atom a)
fromRaw raw orig = case fromRaw' raw of
    Just (AtomGen x) -> case (x,orig) of
        (ConceptNode n tv, ConceptNode _ _ ) -> Just x
        (PredicateNode n , PredicateNode _ ) -> Just x
        (AndLink ao bo tv, AndLink _ _ _   ) -> Just x
        (ListLink l      , ListLink _      ) -> Just x
        _                                    -> Nothing
    Nothing -> Nothing

-- Function to get an Atom back from its general representation (if possible).
fromRaw' :: AtomRaw -> Maybe (AtomGen)
fromRaw' (Node araw n tvraw) = let tv = fromTVRaw <$> tvraw in do
    atype <- fromAtomTypeRaw araw
    case atype of
      ConceptT        -> Just $ AtomGen $ ConceptNode n tv
      PredicateT      -> Just $ AtomGen $ PredicateNode n
      SchemaT         -> Just $ AtomGen $ SchemaNode n
      GroundedSchemaT -> Just $ AtomGen $ GroundedSchemaNode n
      NumberT         -> readMaybe n >>= Just . AtomGen . NumberNode
      _               -> Nothing
    where
        readMaybe :: (Read a) => String -> Maybe a
        readMaybe s = case reads s of
                        [(x, "")] -> Just x
                        _ -> Nothing
fromRaw' (Link araw out tvraw) = let tv = fromTVRaw <$> tvraw in do
    atype <- fromAtomTypeRaw araw
    case (atype,out) of
      (AndT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ AndLink a1 b1 tv
      (OrT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ OrLink a1 b1 tv
      (ImplicationT ,[ar,br]) -> do
        a <- fromRaw' ar
        b <- fromRaw' br
        case (a,b) of
          (AtomGen a1,AtomGen b1) -> Just $ AtomGen $ ImplicationLink a1 b1 tv
      (EquivalenceT ,[ar,br]) -> do
        a <- fromRaw' ar
        b <- fromRaw' br
        case (a,b) of
          (AtomGen a1,AtomGen b1) -> Just $ AtomGen $ EquivalenceLink a1 b1 tv
      (EvaluationT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen PredicateT)
        b <- filt br :: Maybe (Gen ListT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ EvaluationLink a1 b1 tv
      (InheritanceT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ InheritanceLink a1 b1 tv
      (SimilarityT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ SimilarityLink a1 b1 tv
      (MemberT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ MemberLink a1 b1 tv
      (SatisfyingSetT ,[ar]) -> do
        a <- filt ar :: Maybe (Gen PredicateT)
        case a of
          (Gen a1) -> Just $ AtomGen $ SatisfyingSetLink a1
      (ExecutionT ,[ar,br,cr]) -> do
        a <- filt ar :: Maybe (Gen SchemaT)
        b <- filt br :: Maybe (Gen ListT)
        c <- filt br :: Maybe (Gen AtomT)
        case (a,b,c) of
          (Gen a1,Gen b1,Gen c1) -> Just $ AtomGen $ ExecutionLink a1 b1 c1
      (ListT, _     ) -> do
        lnew <- mapM fromRaw' out
        Just $ AtomGen $ ListLink lnew
      _               -> Nothing

filt :: FilterIsChild a => AtomRaw -> Maybe (Gen a)
filt araw = do
    agen <- fromRaw' araw
    case agen of
        (AtomGen at) -> filtIsChild at

-- Constant with the maximum number of parameters in any type of TV.
tvMAX_PARAMS :: Int
tvMAX_PARAMS = 5

-- TV enum type to work with TruthValueTypes from
-- <opencog/atomspace/TruthValue.h> definition.
-- Note: this data type must be always similar to the definition on ../TruthValue.h.
-- The order of enum types MUST be exactly the same on both sites.
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

