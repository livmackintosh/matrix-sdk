{- Copyright 2020 Olivia Mackintosh -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.API.Events where

import Control.Monad.IO.Class ()
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Aeson ()
import Data.Aeson.Text ()
import Data.Maybe ()
import qualified Data.Text as T
import Matrix.API.Config
import Matrix.API.Types
import Network.HTTP.Req
import System.IO ()

type Request a = ReaderT App Req a

sync :: Int -> Maybe T.Text -> Request SyncState
sync timeout since = do
  homeserver <- asks $ configHomeserver . config
  token <- asks $ configToken . config
  let url = apiBase homeserver /: "sync"
      limitFilter = "{\"room\":{\"timeline\":{\"limit\":5}}}" :: T.Text
      options =
        "access_token" =: token
          <> "filter" =: limitFilter
          <> "timeout" =: timeout
          <> "since" `queryParam` since
  r <- lift $ req GET url NoReqBody jsonResponse options
  pure (responseBody r :: SyncState)

send :: T.Text -> T.Text -> Request EventResponse
send txId msg = do
  homeserver <- asks $ configHomeserver . config
  token <- asks $ configToken . config
  roomId <- asks $ configRoomId . config
  let url = apiBase homeserver /: "rooms" /: roomId /: "send" /: "m.room.message" /: txId'
      reqBody = ReqBodyJson $ MessageEvent "m.text" msg
      txId' = txId
      options = "access_token" =: token
  r <- lift $ req PUT url reqBody jsonResponse options
  return (responseBody r :: EventResponse)

apiBase :: T.Text -> Url 'Https
apiBase homeserver = https homeserver /: "_matrix" /: "client" /: "r0"

-- vim: softtabstop=4 expandtab tabstop=4 shiftwidth=4
