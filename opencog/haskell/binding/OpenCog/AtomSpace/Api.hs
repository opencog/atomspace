{-# LANGUAGE ForeignFunctionInterface #-}

module OpenCog.AtomSpace.Api (
      atomspace_new
    , atomspace_delete
    , atomspace_addnode
    , atomspace_print
    , withNewAtomSpace
    ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import OpenCog.AtomSpace.Types

foreign import ccall "AtomSpace_CWrapper.h AtomSpace_new" c_atomspace_new :: IO AtomSpace
atomspace_new :: IO AtomSpace
atomspace_new = c_atomspace_new

foreign import ccall "AtomSpace_CWrapper.h AtomSpace_delete" c_atomspace_delete :: AtomSpace -> IO ()
atomspace_delete :: AtomSpace -> IO ()
atomspace_delete as = c_atomspace_delete as

foreign import ccall "AtomSpace_CWrapper.h AtomSpace_addNode" c_atomspace_addnode :: AtomSpace -> CShort -> CString -> IO ()
atomspace_addnode :: AtomSpace -> CogNode -> IO ()
atomspace_addnode as nod = withCString (node_name nod)
                                       (\str -> c_atomspace_addnode as (fromIntegral $ fromEnum $ node_type nod) str)

foreign import ccall "AtomSpace_CWrapper.h AtomSpace_print" c_atomspace_print :: AtomSpace -> IO ()
atomspace_print :: AtomSpace -> IO ()
atomspace_print = c_atomspace_print

withNewAtomSpace :: (AtomSpace -> IO a) -> IO a
withNewAtomSpace f = do
          at <- atomspace_new
          res <- f at
          atomspace_delete at
          return res

