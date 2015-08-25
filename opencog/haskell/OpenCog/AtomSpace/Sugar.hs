-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This Module defines some util syntactic sugar for embedded atom notation.
module OpenCog.AtomSpace.Sugar (
    stv
  , ctv
  , itv
  , ftv
  , ptv
  , noTv
  , atomList
  , (|>)
  , (\>)
  ) where

import OpenCog.AtomSpace.Inheritance    (type (<~))
import OpenCog.AtomSpace.Types          (TruthVal(..),Gen(..),Atom(..))
import Data.Typeable                    (Typeable)


-- | TruthVal syntactic sugar.
noTv :: Maybe TruthVal
noTv = Nothing

stv :: Double -> Double -> Maybe TruthVal
stv a b = Just $ SimpleTV a b

ctv :: Double -> Double -> Double -> Maybe TruthVal
ctv a b c = Just $ CountTV a b c

itv :: Double -> Double -> Double -> Double -> Double -> Maybe TruthVal
itv a b c d e = Just $ IndefTV a b c d e

ftv :: Double -> Double -> Maybe TruthVal
ftv a b = Just $ FuzzyTV a b

ptv :: Double -> Double -> Double -> Maybe TruthVal
ptv a b c = Just $ ProbTV a b c

-- | 'atomList' is simple sugar notation for listing atoms, using operators '|>'
-- and '\>'. For example, if you want to define a list of atoms:
--
-- @
-- l :: [AtomGen]
-- l = atomList
--       |> ConceptNode "concept1" noTv
--       |> PredicateNode "predicate2" noTv
--       \\> ConceptNode "lastconcept" noTv
-- @
atomList :: (Typeable c) => ([Gen c] -> [Gen c])
atomList = id

infixl 5 |>
infixr 4 \>

-- | '|>' and '\>' operators are provided for easier notation of list of 'Gen'
-- elements when working with atoms of random arity (e.g. 'ListLink').
--
-- * Without sugar:
--
--     @
--     list :: Atom ListT
--     list = ListLink [ Gen (ConceptNode "someConcept1" noTv)
--                     , Gen (PredicateNode "somePredicate1" noTv)
--                     , Gen (PredicateNode "somePredicate2" noTv)
--                     , Gen (ListLink [ Gen (ConceptNode "someConcept2" noTv)
--                                     , Gen (PredicateNode "somePredicate3" noTv)
--                                     ]
--                           )
--                     ]
--     @
--
-- * With sugar:
--
--     @
--     list :: Atom ListT
--     list = ListLink |> ConceptNode "someConcept1" noTv
--                     |> PredicateNode "somePredicate1" noTv
--                     |> PredicateNode "somePredicate2" noTv
--                     \\> ListLink |> ConceptNode "someConcept2" noTv
--                                 \\> PredicateNode "somePredicate3" noTv
--     @
--
(|>) :: (Typeable c,b <~ c) => ([Gen c] -> a) -> Atom b -> ([Gen c] -> a)
f |> at = \l -> f $ (Gen at) : l

(\>) :: (Typeable c,b <~ c) => ([Gen c] -> a) -> Atom b -> a
f \> at = f [Gen at]

