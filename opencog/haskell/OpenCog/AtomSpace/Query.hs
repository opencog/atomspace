-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds                #-}

-- | This Module defines the main functions to interact with the Pattern Matcher.
module OpenCog.AtomSpace.Query (
      cogBind
    ) where

import Control.Monad.IO.Class      (liftIO)
import Foreign.C.Types             (CULong(..))
import OpenCog.AtomSpace.Api       (getByUUID,getWithUUID)
import OpenCog.AtomSpace.Internal  (fromRawGen,toRaw,UUID)
import OpenCog.AtomSpace.Types     (AtomGen,Atom)
import OpenCog.AtomSpace.AtomType  (AtomType(BindT))
import OpenCog.AtomSpace.Env       (AtomSpaceRef(..),AtomSpace,getAtomSpace)

--------------------------------------------------------------------------------

foreign import ccall "PatternMatcher_BindLink"
  c_pmatcher_bindlink :: AtomSpaceRef
                      -> UUID
                      -> IO UUID

-- | 'cogBind' calls the pattern matcher with the given bindLink.
-- (you should insert the bindlink to the atomspace before using this function).
cogBind :: Atom BindT -> AtomSpace (Maybe AtomGen)
cogBind at = do
    m <- getWithUUID $ toRaw at
    case m of
      Just (_,handle) -> do
            asRef <- getAtomSpace
            handleRes <- liftIO $ c_pmatcher_bindlink asRef handle
            mraw <- getByUUID handleRes
            return $ mraw >>= fromRawGen
      Nothing -> return Nothing
