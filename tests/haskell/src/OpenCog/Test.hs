{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeOperators          #-}

module OpenCog.Test where

import OpenCog.Test.TemplateData
import OpenCog.AtomSpace
import Data.Typeable

$(generatedTestData)
