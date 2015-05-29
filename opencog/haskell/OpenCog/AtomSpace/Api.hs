{-# LANGUAGE ForeignFunctionInterface #-}

module OpenCog.AtomSpace.Api (
      asAddNode
    , runOnNewAtomSpace
    , AtomSpace
    ) where

-- Note that I don't export the AtomSpace data constructors nor the asDelete/asNew functions.

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import OpenCog.AtomSpace.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Functor

-- Internal AtomSpace reference to a mutable C++ instance of the AtomSpace class.
newtype AtomSpaceRef = AtomSpaceRef (Ptr AtomSpaceRef)

-- Main Data Type for representing programs working on an AtomSpace.
-- We have to use the IO monad because of the use of FFI for calling c functions for working
-- on a mutable instance of the atomspace, so we have side effects.
type AtomSpace = ReaderT AtomSpaceRef IO

-- Internal functions new and delete, to create and delete C++ instances of the AtomSpace class.
foreign import ccall "AtomSpace_new" c_atomspace_new :: IO AtomSpaceRef
asNew :: IO AtomSpaceRef
asNew = c_atomspace_new

foreign import ccall "AtomSpace_delete" c_atomspace_delete :: AtomSpaceRef -> IO ()
asDelete :: AtomSpaceRef -> IO ()
asDelete = c_atomspace_delete

-- Internal function getAtomSpace.
getAtomSpace :: AtomSpace AtomSpaceRef
getAtomSpace = ask

-- 'asAddNode' calls to the addNode method of the AtomSpace C++ class.
foreign import ccall "AtomSpace_addNode" c_atomspace_addnode :: AtomSpaceRef -> CShort -> CString -> IO CLong
asAddNode :: Node -> AtomSpace Handle
asAddNode nod = do
                    asRef <- getAtomSpace
                    let ntype = fromIntegral $ fromEnum $ nodeType nod
                        fun = \str -> fromIntegral <$> c_atomspace_addnode asRef ntype str
                     in liftIO $ withCString (nodeName nod) fun

-- 'runOnNewAtomSpace' creates a new AtomSpace (C++ object), does some computation over it,
-- and then deletes it.
-- By using bracket, I ensure properly freeing memory in case of exceptions during the computation.
runOnNewAtomSpace :: AtomSpace a -> IO a
runOnNewAtomSpace as = bracket asNew asDelete $ runReaderT as

