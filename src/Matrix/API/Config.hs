{- Copyright 2020 Olivia Mackintosh -}

{-# LANGUAGE OverloadedStrings #-}

module Matrix.API.Config (App(..), Config(..), configAccountName) where

import System.IO (Handle, stdout)
import qualified Data.Text as T

data App = App
 { config :: Config
 , handle :: Handle
 }

data Config = Config
 { configHomeserver ::  T.Text
 , configUsername   ::  T.Text
 , configToken      ::  T.Text
 , configRoomId     ::  T.Text
 } deriving (Show)

configAccountName :: Config -> T.Text
configAccountName c = configUsername c <> ":" <> configHomeserver c
