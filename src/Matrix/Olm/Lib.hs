{- Copyright 2020 Olivia Mackintosh
   Exposes Olm API at higher level of abstraction -}
module Matrix.Olm.Lib where

import Matrix.Olm.Binding

import Control.Monad (join)
import Foreign
import Foreign.C.Types

newtype OlmVer = OlmVer (CUChar, CUChar, CUChar)

instance Show OlmVer where
  show (OlmVer (major,minor,patch)) =
    show major <> "." <> show minor <> "." <> show patch

olmVersion :: IO OlmVer
olmVersion =
  allocaBytes 1 $ \p -> do
    olmGetLibraryVersion p (plusPtr p 1) (plusPtr p 2)
    major <- peek p
    minor <- peek $ plusPtr p 1
    patch <- peek $ plusPtr p 2
    pure $ OlmVer (major, minor, patch)

--newAccount :: IO (Ptr ())
--newAccount = join $ c_olm_account_size >>= \bytes ->
--  allocaBytes (fromEnum bytes) $ \p -> pure $ c_olm_account p

