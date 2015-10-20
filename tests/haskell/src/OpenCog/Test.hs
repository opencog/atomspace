{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}

module OpenCog.Test
(module OpenCog.Test
,module OpenCog.Test.Template)
where

import OpenCog.Test.Template
import Test.SmallCheck.Series
import OpenCog.AtomSpace

instance Monad m => Serial m TruthVal where
    series = cons2 SimpleTV \/ cons3 CountTV \/ cons2 FuzzyTV \/ cons3 ProbTV

$(declareInstanceSerialAtom "../../opencog/atomspace/atom_types.script")

