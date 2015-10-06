-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE DataKinds    #-}

-- | This Module defines some useful data types for proper interaction
-- with the AtomSpace C wrapper library.
-- Intended for internal use only.
module OpenCog.AtomSpace.Internal (
      UUID(..)
    , AtomTypeRaw(..)
    , AtomRaw(..)
    , toRaw
    , fromRaw
    , fromRawGen
    , TVRaw(..)
    , fromTVRaw
    , toTVRaw
    , TVTypeEnum(..)
    , tvMAX_PARAMS
    ) where

import Foreign.C.Types               (CULong(..))
import Data.Functor                  ((<$>))
import Data.Typeable                 (cast,Typeable)
import OpenCog.AtomSpace.Filter      (FilterIsChild(..))
import OpenCog.AtomSpace.AtomType    (AtomType(..),fromAtomTypeRaw,toAtomTypeRaw)
import OpenCog.AtomSpace.Types       (Atom(..),AtomName(..),getType,TruthVal(..),
                                      appGen,Gen(..),AtomGen)

-- Data type to hold atoms's UUID.
type UUID = CULong
type AtomTypeRaw = String

-- Main general atom representation.
data AtomRaw = Link AtomTypeRaw [AtomRaw] (Maybe TVRaw)
             | Node AtomTypeRaw AtomName  (Maybe TVRaw)
        deriving (Eq,Show)

-- Function to convert an Atom to its general representation.
toRaw :: (Typeable a) => Atom a -> AtomRaw
toRaw at = let atype = getType at
           in case at of
    PredicateNode n tv       -> Node atype n $ toTVRaw <$> tv
    ConceptNode n tv         -> Node atype n $ toTVRaw <$> tv
    NumberNode d             -> Node atype (show d) Nothing
    SchemaNode n             -> Node atype n Nothing
    GroundedSchemaNode n     -> Node atype n Nothing
    VariableNode n           -> Node atype n Nothing
    AndLink tv list          -> Link atype (map (appGen toRaw) list) $ toTVRaw <$> tv
    OrLink tv list           -> Link atype (map (appGen toRaw) list) $ toTVRaw <$> tv
    ImplicationLink tv a1 a2 -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    EquivalenceLink tv a1 a2 -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    EvaluationLink tv a1 a2  -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    InheritanceLink tv a1 a2 -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    SimilarityLink tv a1 a2  -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    MemberLink tv a1 a2      -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    SatisfyingSetLink a1     -> Link atype [toRaw a1] Nothing
    ExecutionLink a1 a2 a3   -> Link atype [toRaw a1,toRaw a2,toRaw a3] Nothing
    ListLink list            -> Link atype (map (appGen toRaw) list) Nothing
    SetLink list             -> Link atype (map (appGen toRaw) list) Nothing
    SatisfactionLink a1 a2   -> Link atype [toRaw a1,toRaw a2] Nothing
    ForAllLink tv a1 a2      -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    AverageLink tv a1 a2     -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    QuoteLink a1             -> Link atype [toRaw a1] Nothing
    VariableList list        -> Link atype (map (appGen toRaw) list) Nothing
    BindLink a1 a2 a3        -> Link atype [toRaw a1,toRaw a2,toRaw a3] Nothing
    ContextLink tv c e       -> Link atype [toRaw c,toRaw e] $ toTVRaw <$> tv
    LambdaLink v a	     -> Link atype [toRaw v,toRaw a] Nothing
    NotLink tv a             -> Link atype [toRaw a] $ toTVRaw <$> tv
    SubsetLink tv a1 a2      -> Link atype [toRaw a1,toRaw a2] $ toTVRaw <$> tv
    x                        -> error $ "You should complete the code of toRaw"
                                      ++ " with an instance for: " ++ show x

-- Function to get an Atom back from its general representation (if possible).
fromRaw :: (Typeable a) => AtomRaw -> Atom a -> Maybe (Atom a)
fromRaw raw _ = fromRawGen raw >>= appGen cast

-- Function to get an Atom back from its general representation (if possible).
fromRawGen :: AtomRaw -> Maybe AtomGen
fromRawGen (Node araw n tvraw) = let tv = fromTVRaw <$> tvraw in do
    atype <- fromAtomTypeRaw araw
    case atype of
      ConceptT        -> Just $ Gen $ ConceptNode n tv
      PredicateT      -> Just $ Gen $ PredicateNode n tv
      SchemaT         -> Just $ Gen $ SchemaNode n
      GroundedSchemaT -> Just $ Gen $ GroundedSchemaNode n
      VariableT       -> Just $ Gen $ VariableNode n
      NumberT         -> readMaybe n >>= Just . Gen . NumberNode
      x               -> error $ "You should complete the code of fromRawGen"
                               ++ " with an instance for: " ++ show x
    where
        readMaybe :: (Read a) => String -> Maybe a
        readMaybe s = case reads s of
                        [(x, "")] -> Just x
                        _ -> Nothing
fromRawGen (Link araw out tvraw) = let tv = fromTVRaw <$> tvraw in do
    atype <- fromAtomTypeRaw araw
    case (atype,out) of
      (AndT, _     ) -> do
        lnew <- mapM fromRawGen out
        Just $ Gen $ AndLink tv lnew
      (OrT, _     ) -> do
        lnew <- mapM fromRawGen out
        Just $ Gen $ OrLink tv lnew
      (ImplicationT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen PredicateT)
        b <- filt br :: Maybe (Gen PredicateT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ ImplicationLink tv a1 b1
      (EquivalenceT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen PredicateT)
        b <- filt br :: Maybe (Gen PredicateT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ EquivalenceLink tv a1 b1
      (EvaluationT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen PredicateT)
        b <- filt br :: Maybe (Gen AtomT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ EvaluationLink tv a1 b1
      (InheritanceT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ InheritanceLink tv a1 b1
      (SimilarityT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ConceptT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ SimilarityLink tv a1 b1
      (MemberT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen AtomT)
        b <- filt br :: Maybe (Gen ConceptT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ MemberLink tv a1 b1
      (SatisfyingSetT ,[ar]) -> do
        a <- filt ar :: Maybe (Gen PredicateT)
        case a of
          (Gen a1) -> Just $ Gen $ SatisfyingSetLink a1
      (ExecutionT ,[ar,br,cr]) -> do
        a <- filt ar :: Maybe (Gen SchemaT)
        b <- filt br :: Maybe (Gen AtomT)
        c <- filt br :: Maybe (Gen AtomT)
        case (a,b,c) of
          (Gen a1,Gen b1,Gen c1) -> Just $ Gen $ ExecutionLink a1 b1 c1
      (ListT, _     ) -> do
        lnew <- mapM fromRawGen out
        Just $ Gen $ ListLink lnew
      (SetT, _     ) -> do
        lnew <- mapM fromRawGen out
        Just $ Gen $ SetLink lnew
      (SatisfactionT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen VariableT)
        b <- filt br :: Maybe (Gen LinkT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ SatisfactionLink a1 b1
      (ForAllT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen ListT)
        b <- filt br :: Maybe (Gen ImplicationT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ ForAllLink tv a1 b1
      (AverageT ,[ar,br]) -> do
        a <- filt ar :: Maybe (Gen VariableT)
        b <- filt br :: Maybe (Gen AtomT)
        case (a,b) of
          (Gen a1,Gen b1) -> Just $ Gen $ AverageLink tv a1 b1
      (QuoteT ,[ar]) -> do
        a <- filt ar :: Maybe (Gen AtomT)
        case a of
          (Gen a1) -> Just $ Gen $ QuoteLink a1
      (VariableListT ,_ ) -> do
        lnew <- filtList out :: Maybe [Gen VariableT]
        Just $ Gen $ VariableList lnew
      (BindT ,[ar,br,cr]) -> do
        a <- filt ar :: Maybe (Gen VariableT)
        b <- filt br :: Maybe (Gen AtomT)
        c <- filt cr :: Maybe (Gen AtomT)
        case (a,b,c) of
          (Gen a1,Gen b1,Gen c1) -> Just $ Gen $ BindLink a1 b1 c1
      (ContextT ,[cr,er]) -> do
        c <- filt cr :: Maybe (Gen ConceptT)
        e <- filt er :: Maybe (Gen EvaluationT)
        case (c,e) of
          (Gen c1,Gen e1) -> Just $ Gen $ ContextLink tv c1 e1
      (LambdaT ,[vr,er]) -> do
	v <- filt vr :: Maybe (Gen VariableT)
	e <- filt er :: Maybe (Gen AtomT)
	case (v,e) of
	  (Gen v,Gen e) -> Just $ Gen $ LambdaLink v e
      (NotT ,[ar]) -> do
	a <- filt ar :: Maybe (Gen AtomT)
	case a of
	  (Gen a) -> Just $ Gen $ NotLink tv a
      (SubsetT , [sr,ar]) -> do
	s <- filt sr :: Maybe (Gen AtomT)
	a <- filt sr :: Maybe (Gen AtomT)
	case (s,a) of
	   (Gen s,Gen a) -> Just $ Gen $ SubsetLink tv s a
      x               -> error $ "You should complete the code of fromRawGen"
                               ++ " with an instance for: " ++ show x

filt :: FilterIsChild a => AtomRaw -> Maybe (Gen a)
filt araw = do
    agen <- fromRawGen araw
    case agen of
        (Gen at) -> filtIsChild at

filtList :: FilterIsChild a => [AtomRaw] -> Maybe [Gen a]
filtList = mapM (\raw -> do
                     gen <- fromRawGen raw
                     case gen of
                       Gen g -> filtIsChild g)

instance (Typeable a) => Eq (Atom a) where
    at1 == at2 = toRaw at1 == toRaw at2

instance (Typeable a) => Eq (Gen a) where
    Gen at1 == Gen at2 = Just at1 == cast at2

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
    deriving (Enum,Eq,Show)

data TVRaw = TVRaw TVTypeEnum [Double] deriving (Eq,Show)

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

