{- Copyright 2020 Olivia Mackintosh -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.API.Events where

import Matrix.API.Config
import Matrix.API.Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.Aeson (Value)
import Data.Aeson.Text (encodeToLazyText)
import Data.Maybe (fromMaybe)
import Network.HTTP.Req
import System.IO (hPutStrLn)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy.IO as I
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

sync :: Int -> Maybe String -> Request SyncState
sync timeout since = do
  homeserver <- asks $ configHomeserver.config
  token      <- asks $ configToken.config
  let url         = apiBase homeserver /: "sync"
      limitFilter = "{\"room\":{\"timeline\":{\"limit\":5}}}" :: T.Text
      options     = "access_token"    =: token
                 <> "filter"          =: limitFilter
                 <> "timeout"         =: timeout
                 <> "since" `queryParam` since
  r <- lift $ req GET url NoReqBody jsonResponse options
  pure (responseBody r :: SyncState)


send :: Int -> T.Text -> Request EventResponse
send txId msg = do
  homeserver <- asks $ configHomeserver.config
  token      <- asks $ configToken.config
  roomId     <- asks $ configRoomId.config
  let url = apiBase homeserver /: "rooms" /: roomId /: "send" /: "m.room.message" /: txId'
      reqBody = ReqBodyJson $ MessageEvent "m.text" msg
      txId' = T.pack.show $ txId
      options = "access_token" =: token
  r <- lift $ req PUT url reqBody jsonResponse options
  return (responseBody r :: EventResponse)

apiBase :: T.Text -> Url Https
apiBase homeserver = https homeserver /: "_matrix" /: "client" /: "r0"
