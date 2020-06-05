{- Copyright 2020 Olivia Mackintosh -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.Bot(App(..), start, runPoll) where

import Matrix.API.Config
import Matrix.API.Events(Request, sync, send)
import Matrix.API.Types(SyncState(..), RoomEvent(..), EventResponse, events, timeline, join, event_type)

import Control.Concurrent(forkIO)
import Control.Concurrent.Chan(Chan, newChan, writeChan, readChan)
import Control.Monad(forever)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Trans.Reader(ReaderT, ask, runReaderT)
import Control.Monad.Trans.State()
import Data.Aeson()
import Data.Aeson.Text()
import Network.HTTP.Req

import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML

type Since = T.Text

runRequest :: MonadIO m => r -> ReaderT r Req a -> m a
runRequest a m = runReq defaultHttpConfig (runReaderT m a)

start :: App -> IO ()
start a = do
  q <- newChan
  _ <- forkIO $ runRequest a fullSync >>= \s ->
      print s >> runRequest a (runPoll q (next_batch s))
  runReaderT (runProcess q) a

runPoll :: MonadIO m => Chan SyncState -> T.Text -> ReaderT App m ()
runPoll q since = ask >>= \a -> do
  let l = logger a
  liftIO $ l ("[D][poll]: Delta sync since: " <> since)
  runRequest a $ deltaSync since >>= \s ->
    liftIO (writeChan q s) >> runPoll q (next_batch s)

runProcess :: MonadIO m => Chan SyncState -> ReaderT App m ()
runProcess q = ask >>= \a -> liftIO $ forever $ do
  let l = logger a
      r = (configRoomId.config) a
  readChan q >>= \s ->
    liftIO $ l ("[D][process]: Processing state" <> T.pack (show s)) >>
    mapM_ (runRequest a) (processState (configAccountName $ config a) s r)

processState :: T.Text -> SyncState -> T.Text -> [Request EventResponse]
processState u s r = concat (fmap makeResponse . sieveMsgEvents u <$> getRoomEvents s r)

makeResponse :: RoomEvent -> Request EventResponse
makeResponse r = send (event_id r) "Hi!"

getRoomEvents :: SyncState -> T.Text -> Maybe [RoomEvent]
getRoomEvents s rid = fmap (events.timeline) (HML.lookup rid $ (join.rooms) s)

sieveMsgEvents :: T.Text -> [RoomEvent] -> [RoomEvent]
sieveMsgEvents botUser = filter $ \r ->
    event_type r == "m.room.message"
 && sender r     /= botUser

fullSync :: Request SyncState
fullSync = sync 0 Nothing

deltaSync :: Since -> Request SyncState
deltaSync since = sync 5000 (Just since)

-- vim: softtabstop=4 expandtab tabstop=4 shiftwidth=4
