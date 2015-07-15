-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs, ExistentialQuantification, DataKinds, Rank2Types,
             ScopedTypeVariables, TypeOperators, ConstraintKinds #-}

-- | This Module defines atom filters based on their hierarchy.
module OpenCog.AtomSpace.Filter (
    Gen(..)
  , appGen
  , FilterIsChild(..)
  ) where

import OpenCog.AtomSpace.Inheritance    (AtomType(..),Is)
import OpenCog.AtomSpace.Types          (Atom(..))

-- | 'Gen' groups all the atoms that are children of the atom type a.
data Gen a where
    Gen :: Is b a => Atom b -> Gen a

-- | 'appGen' evaluates a given function with the atom type instance
-- wrapped inside the 'Gen' type.
appGen :: (forall b. Is b a => Atom b -> c) -> Gen a -> c
appGen f (Gen at) = f at

-- | 'FilterIsChild' class defines a filter on the children of atom type 'a'.
class FilterIsChild a where
    filtIsChild :: Atom b -> Maybe (Gen a)

instance FilterIsChild AtomT where
    filtIsChild at = case filtIsChild at :: Maybe (Gen NodeT) of
        Just (Gen at) -> return (Gen at)
        Nothing       -> case filtIsChild at :: Maybe (Gen LinkT) of
            Just (Gen at) -> return (Gen at)
            Nothing       -> Nothing

instance FilterIsChild NodeT where
    filtIsChild at = case at of
        PredicateNode _      -> return $ Gen at
        ConceptNode _ _      -> return $ Gen at
        SchemaNode _         -> return $ Gen at
        GroundedSchemaNode _ -> return $ Gen at
        NumberNode _         -> return $ Gen at
        _                    -> Nothing

instance FilterIsChild LinkT where
    filtIsChild at = case at of
        AndLink _ _ _         -> return $ Gen at
        OrLink _ _ _          -> return $ Gen at
        ImplicationLink _ _ _ -> return $ Gen at
        EquivalenceLink _ _ _ -> return $ Gen at
        EvaluationLink _ _ _  -> return $ Gen at
        InheritanceLink _ _ _ -> return $ Gen at
        SimilarityLink _ _ _  -> return $ Gen at
        MemberLink _ _ _      -> return $ Gen at
        SatisfyingSetLink _   -> return $ Gen at
        ListLink _            -> return $ Gen at
        ExecutionLink _ _ _   -> return $ Gen at
        _                     -> Nothing

instance FilterIsChild PredicateT where
    filtIsChild at = case at of
        PredicateNode _ -> Just $ Gen at
        _               -> Nothing

instance FilterIsChild ConceptT where
    filtIsChild at = case at of
        ConceptNode _ _ -> Just $ Gen at
        _               -> Nothing

instance FilterIsChild NumberT where
    filtIsChild at = case at of
        NumberNode _ -> Just $ Gen at
        _            -> Nothing

instance FilterIsChild GroundedSchemaT where
    filtIsChild at = case at of
        GroundedSchemaNode _ -> Just $ Gen at
        _                    -> Nothing

instance FilterIsChild SchemaT where
    filtIsChild at = case at of
        SchemaNode _         -> Just $ Gen at
        GroundedSchemaNode _ -> Just $ Gen at
        _                    -> Nothing

instance FilterIsChild AndT where
    filtIsChild at = case at of
        AndLink _ _ _ -> return $ Gen at
        _             -> Nothing

instance FilterIsChild OrT where
    filtIsChild at = case at of
        OrLink _ _ _ -> return $ Gen at
        _            -> Nothing

instance FilterIsChild ImplicationT where
    filtIsChild at = case at of
        ImplicationLink _ _ _ -> return $ Gen at
        _                     -> Nothing

instance FilterIsChild EquivalenceT where
    filtIsChild at = case at of
        EquivalenceLink _ _ _ -> return $ Gen at
        _                     -> Nothing

instance FilterIsChild EvaluationT where
    filtIsChild at = case at of
        EvaluationLink _ _ _ -> return $ Gen at
        _                    -> Nothing

instance FilterIsChild InheritanceT where
    filtIsChild at = case at of
        InheritanceLink _ _ _ -> return $ Gen at
        _                     -> Nothing

instance FilterIsChild SimilarityT where
    filtIsChild at = case at of
        SimilarityLink _ _ _ -> return $ Gen at
        _                    -> Nothing

instance FilterIsChild MemberT where
    filtIsChild at = case at of
        MemberLink _ _ _ -> return $ Gen at
        _                -> Nothing

instance FilterIsChild SatisfyingSetT where
    filtIsChild at = case at of
        SatisfyingSetLink _ -> return $ Gen at
        _                   -> Nothing

instance FilterIsChild ListT where
    filtIsChild at = case at of
        ListLink _ -> return $ Gen at
        _          -> Nothing

instance FilterIsChild ExecutionT where
    filtIsChild at = case at of
        ExecutionLink _ _ _ -> return $ Gen at
        _                   -> Nothing

