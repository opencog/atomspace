-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE DataKinds                #-}

-- | This Module defines the main functions to interact with the AtomSpace
-- creating/removing/modifying atoms.
module OpenCog.AtomSpace.Api (
      insert
    , remove
    , get
    , debug
    ) where

import Foreign                       (Ptr)
import Foreign.C.Types               (CULong(..),CInt(..),CDouble(..))
import Foreign.C.String              (CString,withCString)
import Foreign.Marshal.Array         (withArray,allocaArray,peekArray)
import Foreign.Marshal.Utils         (toBool)
import Foreign.Marshal.Alloc         (alloca)
import Foreign.Storable              (peek)
import Data.Functor                  ((<$>))
import Control.Monad.IO.Class        (liftIO)
import OpenCog.AtomSpace.Env         (AtomSpaceRef(..),AtomSpace,getAtomSpace)
import OpenCog.AtomSpace.Internal    (Handle,AtomTypeRaw,AtomRaw(..),TVRaw(..),
                                      toRaw,fromRaw,tvMAX_PARAMS)
import OpenCog.AtomSpace.Types       (Atom(..),AtomName(..),TruthVal(..))
import OpenCog.AtomSpace.Inheritance (type (<~))
import OpenCog.AtomSpace.AtomType    (AtomType(AtomT))

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_debug"
  c_atomspace_debug :: AtomSpaceRef -> IO ()

-- | 'debug' prints the state of the AtomSpace on stderr.
-- (only for debugging purposes)
debug :: AtomSpace ()
debug = do
    asRef <- getAtomSpace
    liftIO $ c_atomspace_debug asRef

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_addNode"
  c_atomspace_addnode :: AtomSpaceRef
                      -> CString
                      -> CString
                      -> IO Handle

insertNode :: AtomTypeRaw -> AtomName -> AtomSpace Handle
insertNode aType aName = do
    asRef <- getAtomSpace
    liftIO $ withCString aType $
       \atype -> withCString aName $
       \aname -> c_atomspace_addnode asRef atype aname

foreign import ccall "AtomSpace_addLink"
  c_atomspace_addlink :: AtomSpaceRef
                      -> CString
                      -> Ptr Handle
                      -> CInt
                      -> IO Handle

insertLink :: AtomTypeRaw -> [AtomRaw] -> AtomSpace Handle
insertLink aType aOutgoing = do
    list <- mapM insertAndGetHandle aOutgoing
    asRef <- getAtomSpace
    liftIO $ withCString aType $
      \atype -> withArray list $
      \lptr -> c_atomspace_addlink asRef atype lptr (fromIntegral $ length list)

insertAndGetHandle :: AtomRaw -> AtomSpace Handle
insertAndGetHandle i = case i of
    Node aType aName mtv     -> do
        h <- insertNode aType aName
        case mtv of -- set truth value after inserting.
            Just tv -> setTruthValue h tv
            Nothing -> return ()
        return h
    Link aType aOutgoing mtv -> do
        h <- insertLink aType aOutgoing
        case mtv of -- set truth value after inserting.
            Just tv -> setTruthValue h tv
            Nothing -> return ()
        return h

-- | 'insert' creates a new atom on the atomspace or updates the existing one.
insert :: Atom a -> AtomSpace ()
insert i = insertAndGetHandle (toRaw i) >> return ()

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_removeAtom"
  c_atomspace_remove :: AtomSpaceRef
                     -> Handle
                     -> IO CInt

-- | 'remove' deletes an atom from the atomspace.
-- Returns True in success or False if it couldn't locate the specified atom.
remove :: Atom a -> AtomSpace Bool
remove i = do
    asRef <- getAtomSpace
    m <- getWithHandle $ toRaw i
    case m of
      Just (_,handle) -> liftIO $ toBool <$> c_atomspace_remove asRef handle
      _               -> return False

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_getNode"
  c_atomspace_getnode :: AtomSpaceRef
                      -> CString
                      -> CString
                      -> Ptr CInt
                      -> IO Handle

getNodeHandle :: AtomTypeRaw -> AtomName -> AtomSpace (Maybe Handle)
getNodeHandle aType aName = do
    asRef <- getAtomSpace
    liftIO $ withCString aType $
      \atype -> withCString aName $
      \aname -> alloca $
      \iptr -> do
          h <- c_atomspace_getnode asRef atype aname iptr
          found <- toBool <$> peek iptr
          return $ if found
                     then Just h
                     else Nothing

getNode :: AtomTypeRaw -> AtomName -> AtomSpace (Maybe (TVRaw,Handle))
getNode aType aName = do
    m <- getNodeHandle aType aName
    case m of
      Nothing -> return Nothing
      Just h  -> do
          tv <- getTruthValue h
          return $ Just (tv,h)


foreign import ccall "AtomSpace_getLink"
  c_atomspace_getlink :: AtomSpaceRef
                      -> CString
                      -> Ptr Handle
                      -> CInt
                      -> Ptr CInt
                      -> IO Handle

getLinkHandle :: AtomTypeRaw -> [Handle] -> AtomSpace (Maybe Handle)
getLinkHandle aType aOutgoing = do
    asRef <- getAtomSpace
    liftIO $ withCString aType $
      \atype -> withArray aOutgoing $
      \lptr -> alloca $
      \iptr -> do
          h <- c_atomspace_getlink asRef atype lptr
                 (fromIntegral $ length aOutgoing) iptr
          found <- toBool <$> peek iptr
          return $ if found
                     then Just h
                     else Nothing

getLink :: AtomTypeRaw -> [Handle] -> AtomSpace (Maybe (TVRaw,Handle))
getLink aType aOutgoing = do
    m <- getLinkHandle aType aOutgoing
    case m of
      Nothing -> return Nothing
      Just h  -> do
          tv <- getTruthValue h
          return $ Just (tv,h)

getWithHandle :: AtomRaw -> AtomSpace (Maybe (AtomRaw,Handle))
getWithHandle i = do
    let onLink :: AtomTypeRaw
               -> [AtomRaw]
               -> AtomSpace (Maybe (TVRaw,Handle,[AtomRaw]))
        onLink aType aOutgoing = do
            ml <- sequence <$> mapM getWithHandle aOutgoing
            case ml of -- ml :: Maybe [(AtomRaw,Handle)]
              Nothing -> return Nothing
              Just l -> do
                res <- getLink aType $ map snd l
                case res of
                  Just (tv,h) -> return $ Just (tv,h,map fst l)
                  _           -> return Nothing
     in
        case i of
          Node aType aName _ -> do
           m <- getNode aType aName
           return $ case m of
             Just (tv,h) -> Just $ (Node aType aName (Just tv),h)
             _           -> Nothing

          Link aType aOutgoing _ -> do
           m <- onLink aType aOutgoing
           return $ case m of
             Just (tv,h,newOutgoing) -> Just $ (Link aType newOutgoing (Just tv), h)
             _                       -> Nothing

-- | 'get' looks for an atom in the atomspace and returns it.
-- (With updated mutable information)
get :: (a <~ AtomT) => Atom a -> AtomSpace (Maybe (Atom a))
get i = do
    m <- getWithHandle $ toRaw i
    return $ case m of
      Just (araw,_) -> fromRaw araw i
      _             -> Nothing

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_getTruthValue"
  c_atomspace_getTruthValue :: AtomSpaceRef
                            -> Handle
                            -> Ptr CDouble
                            -> IO CInt

-- Internal function to get an atom's truth value.
getTruthValue :: Handle -> AtomSpace TVRaw
getTruthValue handle = do
    asRef <- getAtomSpace
    liftIO $ allocaArray tvMAX_PARAMS $
      \lptr -> do
          tvType <- c_atomspace_getTruthValue asRef handle lptr
          l <- peekArray tvMAX_PARAMS lptr
          return $ TVRaw (toEnum $ fromIntegral tvType) (map realToFrac l)

foreign import ccall "AtomSpace_setTruthValue"
  c_atomspace_setTruthValue :: AtomSpaceRef
                            -> Handle
                            -> CInt
                            -> Ptr CDouble
                            -> IO ()

-- Internal function to set an atom's truth value.
setTruthValue :: Handle -> TVRaw -> AtomSpace ()
setTruthValue handle (TVRaw tvtype list) = do
    asRef <- getAtomSpace
    liftIO $ withArray (map realToFrac list) $
      \lptr -> do
          c_atomspace_setTruthValue asRef handle (fromIntegral $ fromEnum tvtype) lptr

