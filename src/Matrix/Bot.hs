{- Copyright 2020 Olivia Mackintosh -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.Bot (App(..), start) where

import Matrix.API.Config
import Matrix.API.Events
import Matrix.API.Types (Request, SyncState(..), MessageEvent(..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Aeson (Value, ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Network.HTTP.Req
import System.IO (Handle, hPutStrLn)

import qualified Data.Text.Lazy.IO as I
import qualified Data.Text as T


start :: ReaderT App IO ()
start = do
  a <- ask
  h <- asks handle
  liftIO $ hPutStrLn h "[I] Starting app"
  liftIO $ hPutStrLn h "[I] Beginning full sync"
  s <- runReq defaultHttpConfig (runReaderT fullSync a)
  lift $ runReq defaultHttpConfig ( runReaderT (runPoll (next_batch s)) a )

runPoll :: T.Text -> ReaderT App Req ()
runPoll since = do
   a <- ask
   h <- asks handle
   liftIO $ hPutStrLn h "[D] Beginning long-poll"
   liftIO $ hPutStrLn h ("[D] Beginning delta sync (since: " ++ T.unpack since ++ ")")
   s <- deltaSync $ T.unpack since
   -- queue delta state for processing
   let nb = next_batch s
   runPoll nb
   pure ()


fullSync :: Request SyncState
fullSync = sync 0 Nothing

deltaSync :: String -> Request SyncState
deltaSync since = sync 5000 (Just since)


writeValue :: (ToJSON a) => a -> IO ()
writeValue v = I.writeFile "output.json" $ encodeToLazyText v

-- vim: softtabstop=4 expandtab tabstop=4 shiftwidth=4
