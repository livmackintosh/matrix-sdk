{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Matrix.Olm.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import Foreign.C.Types
import GHC.Generics (Generic)

type Curve25519 = T.Text

type Ed25519 = T.Text

newtype OlmVer = OlmVer (CUChar, CUChar, CUChar)

instance Show OlmVer where
  show (OlmVer (major, minor, patch)) =
    show major <> "." <> show minor <> "." <> show patch

data IdentityKeys = IdentityKeys {curve25519 :: Curve25519, ed25519 :: Ed25519}
  deriving (Generic, Show)

newtype OneTimeKeys = OneTimeKeys {curve25519 :: H.HashMap T.Text Curve25519}
  deriving (Generic, Show)

instance FromJSON IdentityKeys

instance ToJSON IdentityKeys

instance FromJSON OneTimeKeys

instance ToJSON OneTimeKeys
