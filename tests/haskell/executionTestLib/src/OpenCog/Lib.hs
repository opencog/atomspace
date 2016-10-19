{-# LANGUAGE GADTs #-}
module OpenCog.Lib where

import OpenCog.AtomSpace
import Foreign.C
import Foreign.Ptr

foreign export ccall "someFunc"
    c_func :: Ptr AtomSpaceRef -> Handle -> IO (Handle)

c_func = exportFunction someFunc

someFunc :: Atom -> AtomSpace Atom
someFunc a = pure a
