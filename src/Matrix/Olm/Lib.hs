{- Copyright 2020 Olivia Mackintosh
   Exposes Olm API at higher level of abstraction -}
module Matrix.Olm.Lib where

import Foreign
import Matrix.Olm.C

newtype OlmVer = OlmVer (Word8, Word8, Word8)

instance Show OlmVer where
  show (OlmVer (major,minor,patch)) =
    show major <> "." <> show minor <> "." <> show patch

olmVersion :: IO OlmVer
olmVersion =
  allocaBytes 1 $ \p -> do
    c_olm_get_library_version p (plusPtr p 1) (plusPtr p 2)
    major <- peek p
    minor <- peek $ plusPtr p 1
    patch <- peek $ plusPtr p 2
    pure $ OlmVer (major, minor, patch)
