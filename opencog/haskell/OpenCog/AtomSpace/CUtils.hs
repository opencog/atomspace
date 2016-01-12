module OpenCog.AtomSpace.CUtils where

import Foreign            (Ptr)
import Foreign.C.Types    (CULong(..),CInt(..),CDouble(..))
import Foreign.Marshal.Array         (withArray,allocaArray,peekArray)
import Foreign.Marshal.Alloc         (alloca,free)
import Foreign.Storable              (peek)
import OpenCog.AtomSpace.Internal    (UUID,TVRaw(..),fromTVRaw,tvMAX_PARAMS)
import OpenCog.AtomSpace.Types       (TruthVal(..),Atom(..))
import Debug.Trace

getTVfromC :: (Ptr CInt -> Ptr CDouble -> IO CInt) -> (IO (Maybe TruthVal))
getTVfromC f = do
    alloca $ \tptr -> allocaArray tvMAX_PARAMS $ \lptr -> do
    res <- f tptr lptr
    case res of
        sUCCESS -> do
            tvType <- peek tptr
            l <- peekArray tvMAX_PARAMS lptr
            return $ Just $ fromTVRaw $ TVRaw (toEnum $ fromIntegral tvType) (map realToFrac l)
        _       -> return Nothing
