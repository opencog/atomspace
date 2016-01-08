{-# LANGUAGE GADTs #-}
module OpenCog.Lib where

import OpenCog.AtomSpace
import Foreign.C
import Foreign.Ptr
import Control.Monad.IO.Class

foreign export ccall "someFunc"
    c_func :: Ptr AtomSpaceRef -> UUID -> IO (UUID)

c_func = exportFunction someFunc

someFunc :: AtomGen -> AtomSpace AtomGen
someFunc a = do
    return a
