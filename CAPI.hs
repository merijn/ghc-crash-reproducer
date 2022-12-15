{-# LANGUAGE CApiFFI #-}
module CAPI where

import Foreign (Ptr, FunPtr)
import Foreign.C (CInt(..))

foreign import ccall "sqlite-functions.h &randomFun"
    randomFun :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
