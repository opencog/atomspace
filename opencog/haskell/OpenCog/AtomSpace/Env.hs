-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This Module defines the main environment for the AtomSpace bindings.
module OpenCog.AtomSpace.Env (
      AtomSpace
    , AtomSpaceRef(..)
    , getAtomSpace
    , AtomSpaceObj
    , newAtomSpace
    , onAtomSpace
    , (<:)
    , runOnNewAtomSpace
    ) where

-- Note that I don't export the AtomSpace data constructor nor the
-- c_as_delete/c_as_new functions.

import Foreign                      (Ptr)
import Foreign.ForeignPtr           (ForeignPtr,newForeignPtr,withForeignPtr)
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
newtype AtomSpaceObj = AtomSpaceObj (ForeignPtr AtomSpaceRef)

-- | Main Data Type for representing programs working on an AtomSpace.
-- We have to use the IO monad because of the use of FFI for calling c functions
-- for working on a mutable instance of the atomspace, so we have side effects.
newtype AtomSpace a = AtomSpace (ReaderT AtomSpaceRef IO a)
    deriving (Applicative,Functor,Monad,MonadIO)

-- Internal functions new and delete, to create and delete C++ instances
-- of the AtomSpace class.
foreign import ccall "AtomSpace_new"
  c_as_new :: IO AtomSpaceRef

foreign import ccall "&AtomSpace_delete"
  c_as_delete :: FunPtr (Ptr AtomSpaceRef -> IO ())

-- | 'newAtomSpace' creates a new Atomspace.
newAtomSpace :: IO AtomSpaceObj
newAtomSpace = do
    aref <- c_as_new
    case aref of
      AtomSpaceRef ptr -> AtomSpaceObj <$> newForeignPtr c_as_delete ptr

-- | 'onAtomSpace' runs the specified computation on the atomspace instance
-- provided.
onAtomSpace :: AtomSpaceObj -> AtomSpace a -> IO a
onAtomSpace (AtomSpaceObj aref) (AtomSpace op) = withForeignPtr aref
                                                  (runReaderT op . AtomSpaceRef)

-- | Syntactic sugar for calling the function 'onAtomSpace'.
-- For example, we can write code like this:
--   a <- newAtomSpace
--   b <- newAtomSpace
--
--   a <: insert (ConceptNode "concept1" noTv)
--   a <: debug
--   b <: remove (PredicateNode "predicate1" (stv 1 1))
infixr 0 <:
(<:) :: AtomSpaceObj -> AtomSpace a -> IO a
(<:) = onAtomSpace

-- | 'runOnNewAtomSpace' creates a new AtomSpace (C++ object) and does some
-- computation over it.
runOnNewAtomSpace :: AtomSpace a -> IO a
runOnNewAtomSpace op = do
    as <- newAtomSpace
    onAtomSpace as op

-- Internal function getAtomSpace, to get the actual reference to the atomspace.
getAtomSpace :: AtomSpace AtomSpaceRef
getAtomSpace = AtomSpace ask

