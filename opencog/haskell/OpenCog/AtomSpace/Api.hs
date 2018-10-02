-- GSoC 2015, 2018 - Haskell bindings for OpenCog.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE DataKinds                #-}

-- | This Module defines the main functions to interact with the AtomSpace
-- creating/removing/modifying atoms.
module OpenCog.AtomSpace.Api (
      insert
    , insertAndGetHandle
    , remove
    , get
    , debug
    , getByHandle
    , getWithHandle
    , execute
    , evaluate
    , exportFunction
    , exportPredicate
    ) where

import Foreign                       (Ptr)
import Foreign.C.Types               (CULong(..),CInt(..),CDouble(..))
import Foreign.C.String              (CString,withCString,peekCString)
import Foreign.Marshal.Array         (withArray,allocaArray,peekArray)
import Foreign.Marshal.Utils         (toBool)
import Foreign.Marshal.Alloc
import Foreign.Storable              (peek)
import Data.Functor                  ((<$>))
import Data.Typeable                 (Typeable)
import Data.Maybe                    (fromJust)
import Control.Monad.Trans.Reader    (ReaderT,runReaderT,ask)
import Control.Monad.IO.Class        (liftIO)
import OpenCog.AtomSpace.Env         (AtomSpaceObj(..),AtomSpaceRef(..),(<:),
                                      AtomSpace(..),getAtomSpace,refToObj)
import OpenCog.AtomSpace.Internal    (toTVRaw,fromTVRaw,Handle,HandleSeq,TruthValueP
                                     ,TVRaw(..),tvMAX_PARAMS)
import OpenCog.AtomSpace.Types       (Atom(..),AtomType(..),AtomName(..)
                                     ,TruthVal(..))
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
                      -> Handle
                      -> IO CInt

insertNode :: AtomType -> AtomName -> AtomSpace (Maybe Handle)
insertNode aType aName = do
    asRef <- getAtomSpace
    hptr <- liftIO $ callocBytes 8
    liftIO $ withCString aType $
       \atype -> withCString aName $
       \aname -> do
            res <- c_atomspace_addnode asRef atype aname hptr
            if res == sUCCESS
              then return $ Just hptr
              else return Nothing

foreign import ccall "AtomSpace_addLink"
  c_atomspace_addlink :: AtomSpaceRef
                      -> CString
                      -> HandleSeq
                      -> CInt
                      -> Handle
                      -> IO CInt

insertLink :: AtomType -> [Atom] -> AtomSpace (Maybe Handle)
insertLink aType aOutgoing = do
    mlist <- mapM insertAndGetHandle aOutgoing
    case mapM id mlist of
      Nothing -> return Nothing
      Just list -> do
        asRef <- getAtomSpace
        hptr <- liftIO $ callocBytes 8
        liftIO $ withCString aType $
          \atype -> withArray list $
          \lptr -> do
                res <- c_atomspace_addlink asRef atype lptr (fromIntegral $ length list) hptr
                if res == sUCCESS
                  then return $ Just hptr
                  else return Nothing

insertAndGetHandle :: Atom -> AtomSpace (Maybe Handle)
insertAndGetHandle i = case i of
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
insert :: Atom -> AtomSpace ()
insert i = do
    mh <- insertAndGetHandle i
    case mh of
        Just h -> liftIO $ free h
        Nothing ->  return ()

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_removeAtom"
  c_atomspace_remove :: AtomSpaceRef
                     -> Handle
                     -> IO CInt

-- | 'remove' deletes an atom from the atomspace.
-- Returns True in success or False if it couldn't locate the specified atom.
remove :: Atom -> AtomSpace Bool
remove i = do
    asRef <- getAtomSpace
    m <- getWithHandle i
    case m of
      Just (_,handle) -> do
          res <- liftIO $ c_atomspace_remove asRef handle
          liftIO $ free handle
          return (res == sUCCESS)
      _               -> return False

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_getNode"
  c_atomspace_getnode :: AtomSpaceRef
                      -> CString
                      -> CString
                      -> Handle
                      -> IO CInt

getNodeHandle :: AtomType -> AtomName -> AtomSpace (Maybe Handle)
getNodeHandle aType aName = do
    asRef <- getAtomSpace
    hptr <- liftIO $ callocBytes 8
    liftIO $ withCString aType $
      \atype -> withCString aName $
      \aname -> do
          res <- c_atomspace_getnode asRef atype aname hptr
          let found = res == sUCCESS
          return $ if found
                     then Just hptr
                     else Nothing

getNode :: AtomType -> AtomName -> AtomSpace (Maybe (TruthVal,Handle))
getNode aType aName = do
    m <- getNodeHandle aType aName
    case m of
      Nothing -> return Nothing
      Just h  -> do
          res <- liftIO $ getTruthValue h
          return $ case res of
              Just tv -> Just (tv,h)
              Nothing -> Nothing

foreign import ccall "AtomSpace_getLink"
  c_atomspace_getlink :: AtomSpaceRef
                      -> CString
                      -> HandleSeq
                      -> CInt
                      -> Handle
                      -> IO CInt

getLinkHandle :: AtomType -> [Handle] -> AtomSpace (Maybe Handle)
getLinkHandle aType aOutgoing = do
    asRef <- getAtomSpace
    hptr <- liftIO $ callocBytes 8
    liftIO $ withCString aType $
      \atype -> withArray aOutgoing $
      \lptr -> do
          res <- c_atomspace_getlink asRef atype lptr
                 (fromIntegral $ length aOutgoing) hptr
          let found = res == sUCCESS
          return $ if found
                     then Just hptr
                     else Nothing

getLink :: AtomType -> [Handle] -> AtomSpace (Maybe (TruthVal,Handle))
getLink aType aOutgoing = do
    m <- getLinkHandle aType aOutgoing
    case m of
      Nothing -> return Nothing
      Just h  -> do
          res <- liftIO $ getTruthValue h
          return $ case res of
              Just tv -> Just (tv,h)
              Nothing -> Nothing

getWithHandle :: Atom -> AtomSpace (Maybe (Atom,Handle))
getWithHandle i = do
    let onLink :: AtomType
               -> [Atom]
               -> AtomSpace (Maybe (TruthVal,Handle,[Atom]))
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
             Just (tv,h) -> Just $ (Node aType aName tv,h)
             _           -> Nothing

          Link aType aOutgoing _ -> do
           m <- onLink aType aOutgoing
           return $ case m of
             Just (tv,h,newOutgoing) -> Just $ (Link aType newOutgoing tv, h)
             _                       -> Nothing

-- | 'get' looks for an atom in the atomspace and returns it.
-- (With updated mutable information)
get :: Atom -> AtomSpace (Maybe Atom)
get i = do
    m <- getWithHandle i
    case m of
      Just (araw,handle) -> do liftIO $ free handle
                               return $ Just araw
      _                  -> return $ Nothing

--------------------------------------------------------------------------------

foreign import ccall "Exec_execute"
    c_exec_execute :: AtomSpaceRef
                    -> Handle
                    -> Handle
                    -> IO CInt

execute :: Atom -> AtomSpace (Maybe Atom)
execute atom = do
    m <- getWithHandle atom
    case m of
        Just (_,handle) -> do
            asRef <- getAtomSpace
            hptr <- liftIO $ callocBytes 8
            res <- liftIO $ c_exec_execute asRef handle hptr
            if res == sUCCESS
               then do
                    resAtom <- getByHandle hptr
                    liftIO $ (free handle >> free hptr)
                    return resAtom
               else return Nothing
        _ -> return Nothing

foreign import ccall "Exec_evaluate"
    c_exec_evaluate :: AtomSpaceRef
                    -> Handle
                    -> Ptr CString
                    -> Ptr CDouble
                    -> IO CInt

evaluate :: Atom -> AtomSpace (Maybe TruthVal)
evaluate atom = do
    m <- getWithHandle atom
    case m of
        Just (_,handle) -> do
            asRef <- getAtomSpace
            res <- liftIO $ getTVfromC $ c_exec_evaluate asRef handle
            liftIO $ free handle
            return $ res
        _ -> return Nothing



--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_getAtomByHandle"
  c_atomspace_getAtomByHandle :: AtomSpaceRef
                              -> Handle
                              -> Ptr CInt
                              -> Ptr CString
                              -> Ptr CString
                              -> HandleSeq
                              -> Ptr CInt
                              -> IO CInt

getByHandle :: Handle -> AtomSpace (Maybe Atom)
getByHandle h = do
    asRef <- getAtomSpace
    resTv <- liftIO $ getTruthValue h
    case resTv of
      Nothing -> return Nothing
      Just tv -> do
        res <- liftIO $ alloca $
          \aptr -> alloca $
          \tptr -> alloca $
          \nptr -> alloca $
          \hptr -> alloca $
          \iptr -> do
            res <- c_atomspace_getAtomByHandle asRef h aptr tptr nptr hptr iptr
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
                      outList <- peekArray outLen hptr
                      return $ Just $ Left (atype,outList)
        case res of
            Nothing                     -> return Nothing
            Just (Right (atype,aname))  -> return $ Just $ Node atype aname tv
            Just (Left (atype,outList)) -> do
                mout <- mapM getByHandle outList
                return $ case mapM id mout of
                    Just out -> Just $ Link atype out tv
                    Nothing  -> Nothing

--------------------------------------------------------------------------------

foreign import ccall "TruthValue_getFromAtom"
  c_truthvalue_getFromAtom :: Handle
                            -> Ptr CString
                            -> Ptr CDouble
                            -> IO CInt

-- Internal function to get an atom's truth value.
getTruthValue :: Handle -> IO (Maybe TruthVal)
getTruthValue handle = do
    liftIO $ getTVfromC (c_truthvalue_getFromAtom handle)

foreign import ccall "TruthValue_setOnAtom"
  c_truthvalue_setOnAtom :: Handle
                            -> CString
                            -> Ptr CDouble
                            -> IO CInt

-- Internal function to set an atom's truth value.
setTruthValue :: Handle -> TruthVal -> AtomSpace Bool
setTruthValue handle tv = do
    let (TVRaw tvtype list) = toTVRaw tv
    liftIO $ withArray (map realToFrac list) $
      \lptr -> withCString tvtype $
      \tptr -> do
          res <- c_truthvalue_setOnAtom handle tptr lptr
          return $ res == sUCCESS


foreign import ccall "PTruthValuePtr_fromRaw"
  c_ptruthvalueptr_fromraw :: CString
                            -> Ptr CDouble
                            -> IO TruthValueP

-- Internal function for creating TruthValuePtr* to be returned by GroundedPredicate function
convertToTruthValueP :: TruthVal -> IO TruthValueP
convertToTruthValueP tv = do
    let (TVRaw tvtype list) = toTVRaw tv
    withArray (map realToFrac list) $
      \lptr -> withCString tvtype $
      \tptr -> do
          res <- c_ptruthvalueptr_fromraw tptr lptr
          return res
    

-- Helpfer function for creating function that can be called from C
exportFunction :: (Atom -> AtomSpace Atom) -> Ptr AtomSpaceRef -> Handle -> IO (Handle)
exportFunction f asRef id = do
    as <- refToObj asRef
    (Just atom) <- as <: getByHandle id
    let (AtomSpace op) = f atom
    resAtom <- runReaderT op (AtomSpaceRef asRef)
    (Just resID) <- as <: insertAndGetHandle resAtom
    return resID

-- Helpfer function for creating predicates that can be called from C
exportPredicate :: (Atom -> TruthVal) -> Ptr AtomSpaceRef -> Handle -> IO (TruthValueP)
exportPredicate p asRef id = do
    as <- refToObj asRef
    (Just atom) <- as <: getByHandle id
    convertToTruthValueP $ p atom
