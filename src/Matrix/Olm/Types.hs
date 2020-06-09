{-# LANGUAGE DeriveGeneric #-}
module Matrix.Olm.Types where

import Data.Aeson(ToJSON,FromJSON)
import Foreign.C.Types
import GHC.Generics(Generic)

import qualified Data.Text as T

newtype OlmVer = OlmVer (CUChar, CUChar, CUChar)

instance Show OlmVer where
  show (OlmVer (major,minor,patch)) =
    show major <> "." <> show minor <> "." <> show patch


data IdentityKeys = IdentityKeys
 { curve25519 :: T.Text
 , ed25519 :: T.Text
 } deriving (Generic, Show)

instance FromJSON IdentityKeys
instance ToJSON IdentityKeys
