{- Copyright 2020 Olivia Mackintosh -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.API.Types where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req ()

newtype Response = Response Value deriving (Show, Generic)

newtype SyncResponse = SyncSuccess SyncState

data SyncState = SyncState
  { next_batch :: T.Text,
    rooms :: Rooms
  }
  deriving (Show, Generic)

instance Semigroup SyncState where
  a <> b = SyncState (next_batch b) (rooms a <> rooms b)

instance Monoid SyncState where
  mempty = SyncState "" (Rooms HML.empty)

newtype Rooms = Rooms
  { join :: HML.HashMap T.Text JoinedRoom
  }
  deriving (Show, Generic)

instance Semigroup Rooms where
  a <> b = Rooms (HML.unionWith (<>) (join a) (join b))

newtype JoinedRoom = JoinedRoom
  { timeline :: Timeline
  }
  deriving (Show, Generic)

instance Semigroup JoinedRoom where
  a <> b = JoinedRoom (timeline a <> timeline b)

data Timeline = Timeline
  { events :: [RoomEvent],
    limited :: Bool,
    prev_batch :: T.Text
  }
  deriving (Show, Generic)

instance Semigroup Timeline where
  a <> b =
    Timeline (events a <> events b) (limited a && limited b) (prev_batch a)

data RoomEvent = RoomEvent
  { content :: Object,
    event_type :: T.Text,
    event_id :: T.Text,
    sender :: T.Text,
    origin_server_ts :: Int,
    unsigned :: Value
  }
  deriving (Show, Generic)

data MessageEvent = MessageEvent
  { msgtype :: T.Text,
    body :: T.Text
  }
  deriving (Show, Generic)

data EventResponse
  = NoResponse
  | ResponseSuccess {event_id :: T.Text}
  | ResponseFailure {errcode :: T.Text, error :: T.Text}
  deriving (Show, Generic)

instance ToJSON Response

instance FromJSON Response

instance ToJSON SyncState

instance FromJSON SyncState

instance ToJSON Rooms

instance FromJSON Rooms

instance ToJSON JoinedRoom

instance FromJSON JoinedRoom

instance ToJSON Timeline

instance FromJSON Timeline

-- 'type' is reserved so marshall
-- between 'type' and 'event_type'
fixTypeField :: String -> String
fixTypeField "event_type" = "type"
fixTypeField "type" = "event_type"
fixTypeField s = s

instance ToJSON RoomEvent where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = fixTypeField
        }

instance FromJSON RoomEvent where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fixTypeField
        }

instance ToJSON MessageEvent

instance FromJSON MessageEvent

instance ToJSON EventResponse

instance FromJSON EventResponse where
  parseJSON (Object o) =
    if HML.member "event_id" o
      then ResponseSuccess <$> o .: "event_id"
      else ResponseFailure <$> o .: "errcode" <*> o .: "error"
  parseJSON _ = pure NoResponse

-- vim: softtabstop=4 expandtab tabstop=4 shiftwidth=4
