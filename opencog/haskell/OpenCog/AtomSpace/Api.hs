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
    , getByUUID
    , getWithUUID
    ) where

import Foreign                       (Ptr)
import Foreign.C.Types               (CULong(..),CInt(..),CDouble(..))
import Foreign.C.String              (CString,withCString,peekCString)
import Foreign.Marshal.Array         (withArray,allocaArray,peekArray)
import Foreign.Marshal.Utils         (toBool)
import Foreign.Marshal.Alloc         (alloca,free)
import Foreign.Storable              (peek)
import Data.Functor                  ((<$>))
import Data.Typeable                 (Typeable)
import Control.Monad.IO.Class        (liftIO)
import OpenCog.AtomSpace.Env         (AtomSpaceRef(..),AtomSpace,getAtomSpace)
import OpenCog.AtomSpace.Internal    (UUID,AtomTypeRaw,AtomRaw(..),TVRaw(..),
                                      toRaw,fromRaw,tvMAX_PARAMS)
import OpenCog.AtomSpace.Types       (Atom(..),AtomName(..),TruthVal(..))
import OpenCog.AtomSpace.Inheritance (type (<~))
import OpenCog.AtomSpace.AtomType    (AtomType(AtomT))
import OpenCog.AtomSpace.CUtils

sUCCESS :: CInt
sUCCESS = 0

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
                      -> Ptr UUID
                      -> IO CInt

insertNode :: AtomTypeRaw -> AtomName -> AtomSpace (Maybe UUID)
insertNode aType aName = do
    asRef <- getAtomSpace
    liftIO $ withCString aType $
       \atype -> withCString aName $
       \aname -> alloca $
       \uptr -> do
            res <- c_atomspace_addnode asRef atype aname uptr
            if res == sUCCESS
              then do
                      uuid <- peek uptr
                      return $ Just uuid
              else return Nothing

foreign import ccall "AtomSpace_addLink"
  c_atomspace_addlink :: AtomSpaceRef
                      -> CString
                      -> Ptr UUID
                      -> CInt
                      -> Ptr UUID
                      -> IO CInt

insertLink :: AtomTypeRaw -> [AtomRaw] -> AtomSpace (Maybe UUID)
insertLink aType aOutgoing = do
    mlist <- mapM insertAndGetUUID aOutgoing
    case mapM id mlist of
      Nothing -> return Nothing
      Just list -> do
        asRef <- getAtomSpace
        liftIO $ withCString aType $
          \atype -> withArray list $
          \lptr -> alloca $
          \uptr -> do
                res <- c_atomspace_addlink asRef atype lptr (fromIntegral $ length list) uptr
                if res == sUCCESS
                  then do
                          uuid <- peek uptr
                          return $ Just uuid
                  else return Nothing

insertAndGetUUID :: AtomRaw -> AtomSpace (Maybe UUID)
insertAndGetUUID i = case i of
    Node aType aName tv -> do
        h <- insertNode aType aName
        case h of -- set truth value after inserting.
            Just hand -> setTruthValue hand tv
            _         -> return False
        return h
    Link aType aOutgoing tv -> do
        h <- insertLink aType aOutgoing
        case h of -- set truth value after inserting.
            Just hand -> setTruthValue hand tv
            _         -> return False
        return h

-- | 'insert' creates a new atom on the atomspace or updates the existing one.
insert :: Typeable a => Atom a -> AtomSpace ()
insert i = insertAndGetUUID (toRaw i) >> return ()

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_removeAtom"
  c_atomspace_remove :: AtomSpaceRef
                     -> UUID
                     -> IO CInt

-- | 'remove' deletes an atom from the atomspace.
-- Returns True in success or False if it couldn't locate the specified atom.
remove :: Typeable a => Atom a -> AtomSpace Bool
remove i = do
    asRef <- getAtomSpace
    m <- getWithUUID $ toRaw i
    case m of
      Just (_,handle) -> liftIO $ (==) sUCCESS <$> c_atomspace_remove asRef handle
      _               -> return False

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_getNode"
  c_atomspace_getnode :: AtomSpaceRef
                      -> CString
                      -> CString
                      -> Ptr UUID
                      -> IO CInt

getNodeUUID :: AtomTypeRaw -> AtomName -> AtomSpace (Maybe UUID)
getNodeUUID aType aName = do
    asRef <- getAtomSpace
    liftIO $ withCString aType $
      \atype -> withCString aName $
      \aname -> alloca $
      \hptr -> do
          res <- c_atomspace_getnode asRef atype aname hptr
          let found = res == sUCCESS
          h <- peek hptr
          return $ if found
                     then Just h
                     else Nothing

getNode :: AtomTypeRaw -> AtomName -> AtomSpace (Maybe (TVRaw,UUID))
getNode aType aName = do
    m <- getNodeUUID aType aName
    case m of
      Nothing -> return Nothing
      Just h  -> do
          res <- getTruthValue h
          return $ case res of
              Just tv -> Just (tv,h)
              Nothing -> Nothing

foreign import ccall "AtomSpace_getLink"
  c_atomspace_getlink :: AtomSpaceRef
                      -> CString
                      -> Ptr UUID
                      -> CInt
                      -> Ptr UUID
                      -> IO CInt

getLinkUUID :: AtomTypeRaw -> [UUID] -> AtomSpace (Maybe UUID)
getLinkUUID aType aOutgoing = do
    asRef <- getAtomSpace
    liftIO $ withCString aType $
      \atype -> withArray aOutgoing $
      \lptr -> alloca $
      \hptr -> do
          res <- c_atomspace_getlink asRef atype lptr
                 (fromIntegral $ length aOutgoing) hptr
          let found = res == sUCCESS
          h <- peek hptr
          return $ if found
                     then Just h
                     else Nothing

getLink :: AtomTypeRaw -> [UUID] -> AtomSpace (Maybe (TVRaw,UUID))
getLink aType aOutgoing = do
    m <- getLinkUUID aType aOutgoing
    case m of
      Nothing -> return Nothing
      Just h  -> do
          res <- getTruthValue h
          return $ case res of
              Just tv -> Just (tv,h)
              Nothing -> Nothing

getWithUUID :: AtomRaw -> AtomSpace (Maybe (AtomRaw,UUID))
getWithUUID i = do
    let onLink :: AtomTypeRaw
               -> [AtomRaw]
               -> AtomSpace (Maybe (TVRaw,UUID,[AtomRaw]))
        onLink aType aOutgoing = do
            ml <- sequence <$> mapM getWithUUID aOutgoing
            case ml of -- ml :: Maybe [(AtomRaw,UUID)]
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
             Just (tv,h) -> Just $ (Node aType aName tv,h)
             _           -> Nothing

          Link aType aOutgoing _ -> do
           m <- onLink aType aOutgoing
           return $ case m of
             Just (tv,h,newOutgoing) -> Just $ (Link aType newOutgoing tv, h)
             _                       -> Nothing

-- | 'get' looks for an atom in the atomspace and returns it.
-- (With updated mutable information)
get :: (a <~ AtomT) => Atom a -> AtomSpace (Maybe (Atom a))
get i = do
    m <- getWithUUID $ toRaw i
    return $ case m of
      Just (araw,_) -> fromRaw araw i
      _             -> Nothing

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_getAtomByUUID"
  c_atomspace_getAtomByUUID :: AtomSpaceRef
                              -> UUID
                              -> Ptr CInt
                              -> Ptr CString
                              -> Ptr CString
                              -> Ptr (Ptr UUID)
                              -> Ptr CInt
                              -> IO CInt

getByUUID :: UUID -> AtomSpace (Maybe AtomRaw)
getByUUID h = do
    asRef <- getAtomSpace
    resTv <- getTruthValue h
    case resTv of
      Nothing -> return Nothing
      Just tv -> do
        res <- liftIO $ alloca $
          \aptr -> alloca $
          \tptr -> alloca $
          \nptr -> alloca $
          \hptr -> alloca $
          \iptr -> do
            res <- c_atomspace_getAtomByUUID asRef h aptr tptr nptr hptr iptr
            if res /= sUCCESS
              then return Nothing
              else do
                isNode <- toBool <$> peek aptr
                ctptr <- peek tptr
                atype <- peekCString ctptr
                free ctptr
                if isNode
                  then do
                      cnptr <- peek nptr
                      aname <- peekCString cnptr
                      free cnptr
                      return $ Just $ Right (atype,aname)
                  else do
                      outLen <- fromIntegral <$> peek iptr
                      chptr <- peek hptr
                      outList <- peekArray outLen chptr
                      free chptr
                      return $ Just $ Left (atype,outList)
        case res of
            Nothing                     -> return Nothing
            Just (Right (atype,aname))  -> return $ Just $ Node atype aname tv
            Just (Left (atype,outList)) -> do
                mout <- mapM getByUUID outList
                return $ case mapM id mout of
                    Just out -> Just $ Link atype out tv
                    Nothing  -> Nothing

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_getTruthValue"
  c_atomspace_getTruthValue :: AtomSpaceRef
                            -> UUID
                            -> Ptr CInt
                            -> Ptr CDouble
                            -> IO CInt

-- Internal function to get an atom's truth value.
getTruthValue :: UUID -> AtomSpace (Maybe TVRaw)
getTruthValue handle = do
    asRef <- getAtomSpace
    liftIO $ getTVfromC $ c_atomspace_getTruthValue asRef handle

foreign import ccall "AtomSpace_setTruthValue"
  c_atomspace_setTruthValue :: AtomSpaceRef
                            -> UUID
                            -> CInt
                            -> Ptr CDouble
                            -> IO CInt

-- Internal function to set an atom's truth value.
setTruthValue :: UUID -> TVRaw -> AtomSpace Bool
setTruthValue handle (TVRaw tvtype list) = do
    asRef <- getAtomSpace
    liftIO $ withArray (map realToFrac list) $
      \lptr -> do
          res <- c_atomspace_setTruthValue asRef handle (fromIntegral $ fromEnum tvtype) lptr
          return $ res == sUCCESS

