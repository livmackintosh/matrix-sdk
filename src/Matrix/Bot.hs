{- Copyright 2020 Olivia Mackintosh -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.Bot(App(..), start, runPoll) where

import Matrix.API.Config
import Matrix.API.Events(Request, sync)
import Matrix.API.Types(SyncState(..))

import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.Chan(Chan, newChan, writeChan, readChan)
import Control.Monad(forever)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Trans.Reader(ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State()
import Data.Aeson()
import Data.Aeson.Text()
import Network.HTTP.Req

import qualified Data.Text as T

type Since = T.Text

runRequest :: MonadIO m => r -> ReaderT r Req a -> m a
runRequest a m = runReq defaultHttpConfig (runReaderT m a)

start :: App -> IO ()
start a = do
    q <- newChan
    _ <- forkIO $ runRequest a fullSync >>= \s -> runRequest a $ runPoll q (next_batch s)
    runReaderT (runProcess q) a

runPoll :: MonadIO m => Chan SyncState -> T.Text -> ReaderT App m ()
runPoll q since = do
    asks logger >>= \l -> liftIO $
        l ("[D][poll]: Delta sync since: " <> since)
    a <- ask
    runRequest a $ deltaSync since >>= \s -> liftIO (writeChan q s) >> runPoll q (next_batch s)

runProcess :: MonadIO m => Chan SyncState -> ReaderT App m ()
runProcess q = asks logger >>= \l -> liftIO $ forever $ do
    s <- readChan q
    liftIO $ l ("[D][process]: Processing state" <> T.pack (show s))

fullSync :: Request SyncState
fullSync = sync 0 Nothing

deltaSync :: Since -> Request SyncState
deltaSync since = sync 5000 (Just since)

-- vim: softtabstop=4 expandtab tabstop=4 shiftwidth=4
