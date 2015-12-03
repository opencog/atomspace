-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This Module defines the main environment for the AtomSpace bindings.
module OpenCog.AtomSpace.Env (
      AtomSpace(..)
    , AtomSpaceRef(..)
    , getAtomSpace
    , AtomSpaceObj(..)
    , getParent
    , newAtomSpace
    , onAtomSpace
    , (<:)
    , runOnNewAtomSpace
    , refToObj
    ) where

-- Note that I don't export the AtomSpace data constructor nor the
-- c_as_delete/c_as_new functions.

import Foreign                      (Ptr,nullPtr)
import Foreign.ForeignPtr           (ForeignPtr,newForeignPtr,newForeignPtr_,
                                    withForeignPtr)
import Foreign.Ptr                  (FunPtr)
import Control.Applicative          (Applicative)
import Control.Monad.Trans.Reader   (ReaderT,runReaderT,ask)
import Control.Monad.IO.Class       (MonadIO)
import Data.Functor                 ((<$>))

-- Internal AtomSpace reference to a mutable C++ instance
-- of the AtomSpace class.
newtype AtomSpaceRef = AtomSpaceRef (Ptr AtomSpaceRef)

-- Safe Internal AtomSpace reference to a mutable C++ instance
-- of the AtomSpace class. By using 'ForeignPtr' we associate a finalizer
-- to the AtomSpace pointer, a routine that is invoked when the Haskell storage
-- manager detects that there are no more references left that are pointing to it.
-- | 'AtomSpaceObj' is a specific AtomSpace instance.
data AtomSpaceObj = AtomSpaceObj { actualAS :: (ForeignPtr AtomSpaceRef)
                                 , parentAS :: Maybe AtomSpaceObj
                                 }
    deriving (Eq,Show)

-- This function is necessary because we don't export the internal representation
-- of the data type AtomSpaceObj. We don't want the ForeignPtr to be available
-- outside this module, to the final user.
-- | 'getParent' given an AtomSpace instance returns the parent AtomSpace.
getParent :: AtomSpaceObj -> Maybe AtomSpaceObj
getParent = parentAS

-- | Main Data Type for representing programs working on an AtomSpace.
--
-- Note that AtomSpace is an instance of the type class: MonadIO
--
-- (We have to use the IO monad because of the use of FFI for calling c functions
-- for working on a mutable instance of the atomspace, so we have side effects).
--
-- So, you can lift IO actions inside the monad AtomSpace, through the use of liftIO.
--
newtype AtomSpace a = AtomSpace (ReaderT AtomSpaceRef IO a)
    deriving (Applicative,Functor,Monad,MonadIO)

-- Internal functions new and delete, to create and delete C++ instances
-- of the AtomSpace class.
foreign import ccall "AtomSpace_new"
  c_as_new :: Ptr AtomSpaceRef -> IO AtomSpaceRef

foreign import ccall "&AtomSpace_delete"
  c_as_delete :: FunPtr (Ptr AtomSpaceRef -> IO ())

-- | 'newAtomSpace' creates a new Atomspace, from a optionally given parent atomspace.
newAtomSpace :: Maybe AtomSpaceObj -> IO AtomSpaceObj
newAtomSpace parent = do
    aref <- case parent of
        Just as -> withForeignPtr (actualAS as) c_as_new
        Nothing -> c_as_new nullPtr
    case aref of
      AtomSpaceRef ptr -> do
        actual <- newForeignPtr c_as_delete ptr
        return $ AtomSpaceObj { actualAS = actual
                              , parentAS = parent
                              }

refToObj :: Ptr AtomSpaceRef -> IO AtomSpaceObj
refToObj refptr = do
    actual <- newForeignPtr_ refptr
    return $ AtomSpaceObj { actualAS = actual
                          , parentAS = Nothing
                          }

-- | 'onAtomSpace' runs the specified computation on the atomspace instance
-- provided.
onAtomSpace :: AtomSpaceObj -> AtomSpace a -> IO a
onAtomSpace (AtomSpaceObj { actualAS = aref, parentAS = _ })
            (AtomSpace op) = withForeignPtr aref (runReaderT op . AtomSpaceRef)

-- | Syntactic sugar for calling the function 'onAtomSpace'.
-- For example, we can write code like this:
--
-- @
-- main :: IO ()
-- main = do
--    parentAS <- newAtomSpace Nothing
--    childAS <- newAtomSpace (Just parentAS)
--
--    parentAS <: insert (ConceptNode "GenConcept" noTv)
--    childAS  <: do
--        insert (ConceptNode "PrivateConcept1" (stv 1 1))
--        insert (ConceptNode "PrivateConcept2" (stv 0.5 1))
--    parentAS <: program
--    childAS  <: debug
--    parentAS <: remove (ConceptNode "GenConcept" noTv)
--
-- program :: AtomSpace ()
-- program = do
--     s <- get (ConceptNode "GenConcept" noTv)
--     ...
-- @
--
infixr 0 <:
(<:) :: AtomSpaceObj -> AtomSpace a -> IO a
(<:) = onAtomSpace

-- | 'runOnNewAtomSpace' creates a new AtomSpace (C++ object) and does some
-- computation over it.
runOnNewAtomSpace :: AtomSpace a -> IO a
runOnNewAtomSpace op = do
    as <- newAtomSpace Nothing
    onAtomSpace as op

-- Internal function getAtomSpace, to get the actual reference to the atomspace.
getAtomSpace :: AtomSpace AtomSpaceRef
getAtomSpace = AtomSpace ask

