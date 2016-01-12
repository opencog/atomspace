-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}

-- | This Module defines some util syntactic sugar for embedded atom notation.
module OpenCog.AtomSpace.Sugar (
    stv
  , ctv
  , itv
  , ftv
  , ptv
  , noTv
  ) where

import OpenCog.AtomSpace.Types          (TruthVal(..))

-- | TruthVal syntactic sugar.
noTv :: TruthVal
noTv = stv 1 0

stv :: Double -> Double -> TruthVal
stv a b = SimpleTV a b

ctv :: Double -> Double -> Double -> TruthVal
ctv a b c = CountTV a b c

itv :: Double -> Double -> Double -> Double -> Double -> TruthVal
itv a b c d e = IndefTV a b c d e

ftv :: Double -> Double -> TruthVal
ftv a b = FuzzyTV a b

ptv :: Double -> Double -> Double -> TruthVal
ptv a b c = ProbTV a b c
