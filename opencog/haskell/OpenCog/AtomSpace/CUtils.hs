module OpenCog.AtomSpace.CUtils where

import Foreign                      (Ptr)
import Foreign.C.Types              (CULong(..),CInt(..),CDouble(..))
import Foreign.C.String             (CString,withCString,peekCString)
import Foreign.Marshal.Array        (withArray,allocaArray,peekArray)
import Foreign.Marshal.Alloc        (alloca,free)
import Foreign.Storable             (peek)
import OpenCog.AtomSpace.Internal   (TVRaw(..),fromTVRaw,tvMAX_PARAMS)
import OpenCog.AtomSpace.Types      (TruthVal(..),Atom(..))
import Debug.Trace

getTVfromC :: (Ptr CString -> Ptr CDouble -> IO CInt) -> (IO (Maybe TruthVal))
getTVfromC f = do
    alloca $ \tptr -> allocaArray tvMAX_PARAMS $ \lptr -> do
    res <- f tptr lptr
    case res of
        0 -> do
            ctptr <- peek tptr
            tvType <- peekCString ctptr
            l <- peekArray tvMAX_PARAMS lptr
            return $ Just $ fromTVRaw $ TVRaw tvType (map realToFrac l)
        _       -> return Nothing
