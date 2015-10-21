{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeOperators          #-}

module OpenCog.Test where

import OpenCog.Test.Template
import OpenCog.AtomSpace
import Test.SmallCheck.Series
import Data.Typeable

instance Monad m => Serial m TruthVal where
    series = cons2 SimpleTV \/ cons3 CountTV \/ cons2 FuzzyTV \/ cons3 ProbTV

$(declareInstanceSerialAtom "../../opencog/atomspace/atom_types.script")

uncurry0 = id
uncurry1 = uncurry
uncurry2 f (a,b,c) = (uncurry . uncurry) f ((a,b),c)
myappGen2a :: (forall b. (Typeable bb, b <~ bb) =>
              a -> Atom b -> r) -> (a, Gen bb) -> r
myappGen2a f (a,Gen b) = f a b
myappGen2b :: (forall a b. (Typeable a, Typeable b,
              a <~ aa,b <~ bb) => (Atom a -> Atom b -> r)) ->
              (Gen aa, Gen bb) -> r
myappGen2b f (Gen a,Gen b) = f a b
myappGen3a :: (forall b c. (Typeable b, Typeable c,
              b <~ bb,c <~ cc) => (a -> Atom b -> Atom c -> r)) ->
              (a,Gen bb, Gen cc) -> r
myappGen3a f (a,Gen b,Gen c) = f a b c
myappGen3b :: (forall a b c. (Typeable a,Typeable b, Typeable c,
              a <~ aa,b <~ bb,c <~ cc) =>
              (Atom a -> Atom b -> Atom c -> r)) ->
              (Gen aa,Gen bb, Gen cc) -> r
myappGen3b f (Gen a,Gen b,Gen c) = f a b c
