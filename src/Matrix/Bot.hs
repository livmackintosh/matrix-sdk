{- Copyright 2020 Olivia Mackintosh -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.Bot (App(..), start) where

import Matrix.API.Config
import Matrix.API.Events(Request, sync)
import Matrix.API.Types(SyncState(..))

import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Reader(ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State()
import Data.Aeson()
import Data.Aeson.Text()
import Network.HTTP.Req

import qualified Data.Text as T

type Since = T.Text

runRequest :: MonadIO m => ReaderT r Req a -> r -> m a
runRequest m a = runReq defaultHttpConfig (runReaderT m a)

start :: ReaderT App IO ()
start = do
  asks logger >>= \l -> liftIO $
    l "[I] Starting app" >>
    l "[I] Beginning full sync"
  app <- ask
  s <- runRequest fullSync app
  lift $ runRequest (runPoll (next_batch s)) app

runPoll :: Since -> ReaderT App Req ()
runPoll since = do
   asks logger >>= \l -> liftIO $
      l "[D] Beginning long-poll" >>
      l ("[D] Beginning delta sync since: " <> since)
   deltaSync since >>= \s -> runPoll $ next_batch s

fullSync :: Request SyncState
fullSync = sync 0 Nothing

deltaSync :: Since -> Request SyncState
deltaSync since = sync 5000 (Just since)

-- vim: softtabstop=4 expandtab tabstop=4 shiftwidth=4
