-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds                #-}

-- | This Module defines the main functions to interact with the Pattern Matcher.
module OpenCog.AtomSpace.Query where

import Control.Monad.IO.Class      (liftIO)
import Foreign                     (Ptr)
import Foreign.C.Types             (CULong(..),CInt(..),CDouble(..))
import Foreign.C.String             (CString,withCString,peekCString)
import OpenCog.AtomSpace.Api       (getByHandle,getWithHandle)
import OpenCog.AtomSpace.Types     (TruthVal,Atom(..))
import OpenCog.AtomSpace.Env       (AtomSpaceRef(..),AtomSpace,getAtomSpace)
import OpenCog.AtomSpace.CUtils
import OpenCog.AtomSpace.Internal  (Handle)

--------------------------------------------------------------------------------

foreign import ccall "PatternMatcher_BindLink"
  c_pmatcher_bindlink :: AtomSpaceRef
                      -> Handle
                      -> IO Handle

-- | 'cogBind' calls the pattern matcher with the given bindLink.
-- (you should insert the bindlink to the atomspace before using this function).
cogBind :: Atom -> AtomSpace (Maybe Atom)
cogBind at = do
    m <- getWithHandle at
    case m of
      Just (_,handle) -> do
            asRef <- getAtomSpace
            handleRes <- liftIO $ c_pmatcher_bindlink asRef handle
            matom <- getByHandle handleRes
            return $ matom
      Nothing -> return Nothing

foreign import ccall "PatternMatcher_SatisfactionLink"
  c_pmatcher_satisfactionlink :: AtomSpaceRef
                      -> Handle
                      -> Ptr CString
                      -> Ptr CDouble
                      -> IO CInt

cogSatisfy :: Atom -> AtomSpace (Maybe TruthVal)
cogSatisfy at = do
    m <- getWithHandle at
    case m of
        Just (_,handle) -> do
            asRef <- getAtomSpace
            res <- liftIO $ getTVfromC $ c_pmatcher_satisfactionlink asRef handle
            return res
        Nothing -> return Nothing

