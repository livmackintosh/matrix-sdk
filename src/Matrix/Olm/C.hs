{- Copyright 2020 Olivia Mackintosh -}
{-# LANGUAGE ForeignFunctionInterface #-}

module Matrix.Olm.C(c_olm_get_library_version) where

import Foreign

foreign import ccall unsafe "olm/olm.h olm_get_library_version" c_olm_get_library_version ::
  Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
