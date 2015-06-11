{-# LANGUAGE ForeignFunctionInterface , GADTs #-}

module OpenCog.AtomSpace.Api (
      insert
    , remove
    , get
    , debug
    , runOnNewAtomSpace
    , AtomSpace
    ) where

-- Note that I don't export the AtomSpace data constructors nor the
-- asDelete/asNew functions.

import Foreign                      (Ptr)
import Foreign.C.Types              (CULong(..),CInt(..))
import Foreign.C.String             (CString,withCString)
import Foreign.Marshal.Array        (withArray)
import Foreign.Marshal.Utils        (toBool)
import Foreign.Marshal.Alloc        (alloca)
import Foreign.Storable             (peek)
import Control.Exception            (bracket)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Reader   (ReaderT,runReaderT,ask)
import Data.Functor                 ((<$>))
import OpenCog.AtomSpace.Types      (Atom(..),AtomName(..),TruthVal(..),
                                     appAtomGen,AtomGen(..))

-- Internal AtomSpace reference to a mutable C++ instance
-- of the AtomSpace class.
newtype AtomSpaceRef = AtomSpaceRef (Ptr AtomSpaceRef)

-- Main Data Type for representing programs working on an AtomSpace.
-- We have to use the IO monad because of the use of FFI for calling c functions
-- for working on a mutable instance of the atomspace, so we have side effects.
type AtomSpace = ReaderT AtomSpaceRef IO

-- Internal functions new and delete, to create and delete C++ instances
-- of the AtomSpace class.
foreign import ccall "AtomSpace_new"
  c_atomspace_new :: IO AtomSpaceRef
asNew :: IO AtomSpaceRef
asNew = c_atomspace_new

foreign import ccall "AtomSpace_delete"
  c_atomspace_delete :: AtomSpaceRef -> IO ()
asDelete :: AtomSpaceRef -> IO ()
asDelete = c_atomspace_delete

-- 'runOnNewAtomSpace' creates a new AtomSpace (C++ object), does some
-- computation over it, and then deletes it.
-- By using bracket, I ensure properly freeing memory in case of exceptions
-- during the computation.
runOnNewAtomSpace :: AtomSpace a -> IO a
runOnNewAtomSpace as = bracket asNew asDelete $ runReaderT as

-- Internal function getAtomSpace, to get the actual reference to the atomspace.
getAtomSpace :: AtomSpace AtomSpaceRef
getAtomSpace = ask

--------------------------------------------------------------------------------

type Handle = CULong
type AtomType = String
data AtomRaw = Link AtomType [AtomRaw] (Maybe TruthVal)
             | Node AtomType AtomName  (Maybe TruthVal)

toRaw :: Atom a -> AtomRaw
toRaw i = case i of
    Predicate n  -> Node "PredicateNode" n Nothing
    And a1 a2 tv -> Link "AndLink" [toRaw a1,toRaw a2] tv
    Concept n    -> Node "ConceptNode" n Nothing
    List list    -> Link "ListLink" (map (appAtomGen toRaw) list) Nothing
    _            -> undefined

fromRaw :: AtomRaw -> Atom a -> Maybe (Atom a)
fromRaw raw orig = case (raw,orig) of
    (Node "ConceptNode" n _    , Concept _   ) -> Just $ Concept n
    (Node "PredicateNode" n _  , Predicate _ ) -> Just $ Predicate n
    (Link "AndLink" [ar,br] tv , And ao bo _ ) -> do
        a <- fromRaw ar ao
        b <- fromRaw br bo
        Just $ And a b tv
    (Link "ListLink" lraw _    , List lorig  ) -> do
        lnew <- if length lraw == length lorig
                 then sequence $ zipWith (\raw orig -> 
                                    appAtomGen
                                    ((<$>) AtomGen . fromRaw raw) orig)
                                    lraw lorig
                 else Nothing
        Just $ List lnew
    _                                               -> Nothing -- undefined

--------------------------------------------------------------------------------

-- Debug function to print the atomspace on stderr.
foreign import ccall "AtomSpace_debug"
  c_atomspace_debug :: AtomSpaceRef -> IO ()

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

insertNode :: AtomType -> AtomName -> AtomSpace Handle
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

insertLink :: AtomType -> [AtomRaw] -> AtomSpace Handle
insertLink aType aOutgoing = do
    list <- mapM insertAndGetHandle aOutgoing
    asRef <- getAtomSpace
    liftIO $ withCString aType $
      \atype -> withArray list $
      \lptr -> c_atomspace_addlink asRef atype lptr (fromIntegral $ length list)

insertAndGetHandle :: AtomRaw -> AtomSpace Handle
insertAndGetHandle i = case i of
    Node aType aName tv     -> insertNode aType aName
                               -- TODO: After getting handler set truthvalue!
    Link aType aOutgoing tv -> insertLink aType aOutgoing
                               -- TODO: After getting handler set truthvalue!

-- Function to insert an atom to the atomspace.
insert :: Atom a -> AtomSpace ()
insert i = insertAndGetHandle (toRaw i) >> return ()

--------------------------------------------------------------------------------

foreign import ccall "AtomSpace_removeAtom"
  c_atomspace_remove :: AtomSpaceRef
                     -> Handle
                     -> IO CInt

-- Function to remove an atom from the atomspace.
remove :: Atom a -> AtomSpace Bool
remove i = do
    asRef <- getAtomSpace
    m <- getWithHandle $ toRaw i -- TODO: Make more efficiently this
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

getNodeHandle :: AtomType -> AtomName -> AtomSpace (Maybe Handle)
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

getNode :: AtomType -> AtomName -> AtomSpace (Maybe (TruthVal,Handle))
getNode aType aName = do
    m <- getNodeHandle aType aName
    return $ case m of
      Nothing -> Nothing
      Just h  -> Just (undefined,h)
              -- TODO: After getting handler, get actual truthvalue!


foreign import ccall "AtomSpace_getLink"
  c_atomspace_getlink :: AtomSpaceRef
                      -> CString
                      -> Ptr Handle
                      -> CInt
                      -> Ptr CInt
                      -> IO Handle

getLinkHandle :: AtomType -> [Handle] -> AtomSpace (Maybe Handle)
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

getLink :: AtomType -> [Handle] -> AtomSpace (Maybe (TruthVal,Handle))
getLink aType aOutgoing = do
    m <- getLinkHandle aType aOutgoing
    return $ case m of
      Nothing -> Nothing
      Just h  -> Just (undefined,h)
              -- TODO: After getting handler, get actual truthvalue!

getWithHandle :: AtomRaw -> AtomSpace (Maybe (AtomRaw,Handle))
getWithHandle i = do
    let onLink :: AtomType
               -> [AtomRaw]
               -> AtomSpace (Maybe (TruthVal,Handle,[AtomRaw]))
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

-- Function to get an atom from the atomspace.
get :: Atom a -> AtomSpace (Maybe (Atom a))
get i = do
    m <- getWithHandle $ toRaw i
    return $ case m of
      Just (araw,_) -> fromRaw araw i
      _             -> Nothing

--------------------------------------------------------------------------------
