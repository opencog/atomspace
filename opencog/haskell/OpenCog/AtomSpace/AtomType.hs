-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE AutoDeriveTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}

-- | This Module defines the different Atom Types and some utils functions.
module OpenCog.AtomSpace.AtomType (
    AtomType(..)
  , Up(..)
  , Down(..)
  , fromAtomTypeRaw
  , toAtomTypeRaw
  ) where

-- | 'AtomType' kind groups all atom types.
data AtomType = AtomT
              | NodeT
              | LinkT
              | PredicateT
              | ConceptT
              | SchemaT
              | GroundedSchemaT
              | NumberT
              | AndT
              | OrT
              | ImplicationT
              | EquivalenceT
              | EvaluationT
              | InheritanceT
              | SimilarityT
              | MemberT
              | SatisfyingSetT
              | ListT
              | ExecutionT

toAtomTypeRaw :: AtomType -> String
toAtomTypeRaw at = case at of
    PredicateT      -> "PredicateNode"
    AndT            -> "AndLink"
    OrT             -> "OrLink"
    ImplicationT    -> "ImplicationLink"
    EquivalenceT    -> "EquivalenceLink"
    EvaluationT     -> "EvaluationLink"
    ConceptT        -> "ConceptNode"
    InheritanceT    -> "InheritanceLink"
    SimilarityT     -> "SimilarityLink"
    MemberT         -> "MemberLink"
    SatisfyingSetT  -> "SatisfyingSetLink"
    NumberT         -> "NumberNode"
    ListT           -> "ListLink"
    SchemaT         -> "SchemaNode"
    GroundedSchemaT -> "GroundedSchemaNode"
    ExecutionT      -> "ExecutionLink"


fromAtomTypeRaw :: String -> Maybe AtomType
fromAtomTypeRaw s = case s of
    "PredicateNode"      -> Just PredicateT
    "AndLink"            -> Just AndT
    "OrLink"             -> Just OrT
    "ImplicationLink"    -> Just ImplicationT
    "EquivalenceLink"    -> Just EquivalenceT
    "EvaluationLink"     -> Just EvaluationT
    "ConceptNode"        -> Just ConceptT
    "InheritanceLink"    -> Just InheritanceT
    "SimilarityLink"     -> Just SimilarityT
    "MemberLink"         -> Just MemberT
    "SatisfyingSetLink"  -> Just SatisfyingSetT
    "NumberNode"         -> Just NumberT
    "ListLink"           -> Just ListT
    "SchemaNode"         -> Just SchemaT
    "GroundedSchemaNode" -> Just GroundedSchemaT
    "ExecutionLink"      -> Just ExecutionT
    _                    -> Nothing

-- | 'Up' given an atom type returns a list with its parent atom types.
type family Up a :: [AtomType] where
    Up AtomT           = '[]
    Up NodeT           = '[AtomT]
    Up LinkT           = '[AtomT]
    Up ConceptT        = '[NodeT]
    Up SchemaT         = '[NodeT]
    Up PredicateT      = '[NodeT]
    Up NumberT         = '[NodeT]
    Up GroundedSchemaT = '[SchemaT]
    Up ExecutionT      = '[LinkT]
    Up AndT            = '[LinkT]
    Up OrT             = '[LinkT]
    Up ImplicationT    = '[LinkT]
    Up EquivalenceT    = '[LinkT]
    Up EvaluationT     = '[LinkT]
    Up InheritanceT    = '[LinkT]
    Up SimilarityT     = '[LinkT]
    Up MemberT         = '[LinkT]
    Up SatisfyingSetT  = '[LinkT]
    Up ListT           = '[LinkT]

-- | 'Down' given an atom type returns a list with its children atom types.
type family Down a :: [AtomType] where
    Down AtomT           = '[NodeT,LinkT]
    Down NodeT           = '[ ConceptT
                            , SchemaT
                            , PredicateT
                            , NumberT
                            , GroundedSchemaT
                            ]
    Down LinkT           = '[ ExecutionT
                            , AndT
                            , OrT
                            , ImplicationT
                            , EquivalenceT
                            , EvaluationT
                            , InheritanceT
                            , SimilarityT
                            , MemberT
                            , SatisfyingSetT
                            , ListT
                            ]
    Down ConceptT        = '[]
    Down SchemaT         = '[GroundedSchemaT]
    Down PredicateT      = '[]
    Down NumberT         = '[]
    Down GroundedSchemaT = '[]
    Down ExecutionT      = '[]
    Down AndT            = '[]
    Down OrT             = '[]
    Down ImplicationT    = '[]
    Down EquivalenceT    = '[]
    Down EvaluationT     = '[]
    Down InheritanceT    = '[]
    Down SimilarityT     = '[]
    Down MemberT         = '[]
    Down SatisfyingSetT  = '[]
    Down ListT           = '[]

