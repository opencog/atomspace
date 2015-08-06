-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE DataKinds    #-}

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
import Data.Typeable                 (cast,Typeable)
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
        deriving Eq

-- Function to convert an Atom to its general representation.
toRaw :: (Typeable a) => Atom a -> AtomRaw
toRaw at = let atype = toAtomTypeRaw $ getType at
           in case at of
    PredicateNode n tv       -> Node atype n $ toTVRaw <$> tv
    ConceptNode n tv         -> Node atype n $ toTVRaw <$> tv
    NumberNode d             -> Node atype (show d) Nothing
    SchemaNode n             -> Node atype n Nothing
    GroundedSchemaNode n     -> Node atype n Nothing
    VariableNode n           -> Node atype n Nothing
    AndLink tv a1 a2         -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    OrLink tv a1 a2          -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    ImplicationLink tv a1 a2 -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    EquivalenceLink tv a1 a2 -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    EvaluationLink tv a1 a2  -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    InheritanceLink tv a1 a2 -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    SimilarityLink tv a1 a2  -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    MemberLink tv a1 a2      -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    SatisfyingSetLink a1     -> Link atype [toRaw a1] Nothing
    ExecutionLink a1 a2 a3   -> Link atype [toRaw a1,toRaw a2,toRaw a3] Nothing
    ListLink list            -> Link atype (map (appAtomGen toRaw) list) Nothing
    SatisfactionLink a1 a2   -> Link atype [toRaw a1,toRaw a2] Nothing
    ForAllLink tv a1 a2      -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    AverageLink tv a1 a2     -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    _                        -> undefined

-- Function to get an Atom back from its general representation (if possible).
fromRaw :: (Typeable a) => AtomRaw -> Atom a -> Maybe (Atom a)
fromRaw raw _ = fromRaw' raw >>= appAtomGen cast

-- Function to get an Atom back from its general representation (if possible).
fromRaw' :: AtomRaw -> Maybe (AtomGen)
fromRaw' (Node araw n tvraw) = let tv = fromTVRaw <$> tvraw in do
    atype <- fromAtomTypeRaw araw
    case atype of
      ConceptT        -> Just $ AtomGen $ ConceptNode n tv
      PredicateT      -> Just $ AtomGen $ PredicateNode n tv
      SchemaT         -> Just $ AtomGen $ SchemaNode n
      GroundedSchemaT -> Just $ AtomGen $ GroundedSchemaNode n
      VariableT       -> Just $ AtomGen $ VariableNode n
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
        a <- filt ar :: Maybe (Gen AtomT)
        b <- filt br :: Maybe (Gen AtomT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ AndLink tv a1 b1
      (OrT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen AtomT)
        b <- filt br :: Maybe (Gen AtomT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ OrLink tv a1 b1
      (ImplicationT ,[ar,br]) -> do
        a <- fromRaw' ar
        b <- fromRaw' br
        case (a,b) of
          (AtomGen a1,AtomGen b1) -> Just $ AtomGen $ ImplicationLink tv a1 b1
      (EquivalenceT ,[ar,br]) -> do
        a <- fromRaw' ar
        b <- fromRaw' br
        case (a,b) of
          (AtomGen a1,AtomGen b1) -> Just $ AtomGen $ EquivalenceLink tv a1 b1
      (EvaluationT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen PredicateT)
        b <- filt br :: Maybe (Gen ListT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ EvaluationLink tv a1 b1
      (InheritanceT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ InheritanceLink tv a1 b1
      (SimilarityT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ SimilarityLink tv a1 b1
      (MemberT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen NodeT)
        b <- filt br :: Maybe (Gen NodeT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ MemberLink tv a1 b1
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
      (SatisfactionT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen VariableT)
        b <- filt br :: Maybe (Gen LinkT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ SatisfactionLink a1 b1
      (ForAllT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ListT)
        b <- filt br :: Maybe (Gen ImplicationT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ ForAllLink tv a1 b1
      (AverageT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen VariableT)
        b <- filt br :: Maybe (Gen AtomT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ AtomGen $ AverageLink tv a1 b1
      _               -> Nothing

filt :: FilterIsChild a => AtomRaw -> Maybe (Gen a)
filt araw = do
    agen <- fromRaw' araw
    case agen of
        (AtomGen at) -> filtIsChild at

instance (Typeable a) => Eq (Atom a) where
    at1 == at2 = toRaw at1 == toRaw at2

instance Eq AtomGen where
    AtomGen at1 == AtomGen at2 = Just at1 == cast at2

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
    deriving (Enum,Eq)

data TVRaw = TVRaw TVTypeEnum [Double] deriving Eq

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

