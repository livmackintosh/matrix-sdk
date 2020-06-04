{- Copyright 2020 Olivia Mackintosh -}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.API.Config (App(..), Config(..), configAccountName) where

import Matrix.API.Types()
import System.IO()
import qualified Data.Text as T

data App = App
 { config :: Config
 , logger :: T.Text -> IO ()
 }

data Config = Config
 { configHomeserver ::  T.Text
 , configUsername   ::  T.Text
 , configToken      ::  T.Text
 , configRoomId     ::  T.Text
 } deriving (Show)

configAccountName :: Config -> T.Text
configAccountName c = configUsername c <> ":" <> configHomeserver c

-- vim: softtabstop=4 expandtab tabstop=4 shiftwidth=4
