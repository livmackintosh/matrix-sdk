{- Copyright 2020 Olivia Mackintosh -}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Matrix.API.Config (Config(..))
import Matrix.Bot (App(..), start, handle, config)

import Control.Monad.Trans.Reader (runReaderT)
import Network.HTTP.Req
import Options.Applicative (Parser, (<**>))
import System.IO (stdout)

import qualified Options.Applicative as O

optParser :: Parser Config
optParser = Config
         <$> O.strOption
             ( O.long "homeserver"
            <> O.help "Homeserver to connect to"
            <> O.showDefault
            <> O.value "matrix.org"
            <> O.metavar "HOSTNAME")
         <*> O.strOption
             ( O.long "user"
            <> O.help "Username to connect as (e.g. \"example\")"
            <> O.metavar "USERNAME" )
         <*> O.strOption
             ( O.long "auth"
            <> O.help "Valid access token"
            <> O.metavar "TOKEN" )
         <*> O.strOption
             ( O.long "room"
            <> O.help "Room ID to listen to (e.g. \"!abcde:@matrix.org\")"
            <> O.metavar "ROOM_ID" )

app :: Config -> IO ()
app c = runReaderT start $ App c stdout

main :: IO ()
main = app =<< O.execParser opts
  where
    opts = O.info (optParser <**> O.helper)
      ( O.fullDesc
     <> O.progDesc "Starts the chatbot as USERNAME"
     <> O.header "matrix-bot - A [matrix] chatbot written in Haskell" )
