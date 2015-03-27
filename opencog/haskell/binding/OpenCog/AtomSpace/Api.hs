{-# LANGUAGE ForeignFunctionInterface #-}

module OpenCog.AtomSpace.Api (
      asAddNode
    , asPrint
    , runOnNewAtomSpace
    , AtomSpace
    ) where

--Note that I don't export the AtomSpace data constructors nor the asDelete/asNew functions.

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import OpenCog.AtomSpace.Types
import Control.Monad.IO.Class

-- Internal AtomSpace reference to a mutable C++ instance of the AtomSpace class.
newtype AtomSpaceRef = AtomSpaceRef (Ptr AtomSpaceRef)

-- Main Data Type for representing programs working on an AtomSpace.
-- The run function inside means: "Once you have a reference to an AtomSpace in memory,
-- working on it reduces to performing IO actions"
-- We have to use the IO monad because of the use of FFI for calling c functions for working
-- on a mutable instance of the atomspace, so we have side effects.
data AtomSpace a = AtomSpace {run :: AtomSpaceRef -> IO a}

instance Monad AtomSpace where
    m1 >>= f = AtomSpace $ \asRef -> do
                                       a <- run m1 asRef
                                       run (f a) asRef
    return a = AtomSpace (\_ -> return a)

instance MonadIO AtomSpace where
    liftIO m = AtomSpace (\_ -> m)

-- Internal Functions new and delete, to create and delete C++ instances of the AtomSpace class.
foreign import ccall "AtomSpace_CWrapper.h AtomSpace_new"
               c_atomspace_new :: IO AtomSpaceRef
asNew :: IO AtomSpaceRef
asNew = c_atomspace_new

foreign import ccall "AtomSpace_CWrapper.h AtomSpace_delete"
               c_atomspace_delete :: AtomSpaceRef -> IO ()
asDelete :: AtomSpaceRef -> IO ()
asDelete = c_atomspace_delete

-- 'asAddNode' This function calls to the addNode method of the AtomSpace C++ class.
foreign import ccall "AtomSpace_CWrapper.h AtomSpace_addNode"
               c_atomspace_addnode :: AtomSpaceRef -> CShort -> CString -> IO ()
asAddNode :: Node -> AtomSpace ()
asAddNode nod = AtomSpace (\asRef -> let ntype = fromIntegral $ fromEnum $ nodeType nod
                                         fun = \str -> c_atomspace_addnode asRef ntype str
                                      in withCString (nodeName nod) fun)

-- 'asPrint' calls to the print method of the AtomSpace C++ class.
foreign import ccall "AtomSpace_CWrapper.h AtomSpace_print"
               c_atomspace_print :: AtomSpaceRef -> IO ()
asPrint :: AtomSpace ()
asPrint = AtomSpace c_atomspace_print

-- 'runOnNewAtomSpace' creates a new AtomSpace (C++ object), does some computation run_ over it,
-- and then deletes the AtomSpace.
-- By using bracket, I ensure properly freeing memory in case of exceptions in the computation run_.
runOnNewAtomSpace :: AtomSpace a -> IO a
runOnNewAtomSpace (AtomSpace run_) = bracket asNew asDelete run_

