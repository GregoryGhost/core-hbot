{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module HBot.Core.Cmd where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text
import Data.Typeable

type Response = String
type SourceCmd = String
type SourcePayload = String
type ExpectedCmd = String
data Cmd = Cmd
    {
        getCmd :: SourceCmd
        , getArgs :: CmdArgs
    } deriving Show
type CmdArg = String
type CmdArgs = [CmdArg]

data BotCmd c = BotCmd { cmd :: c, args :: CmdArgs}

data ParseError = NoCmd | UnknownBotCmd
    deriving Typeable

instance Show ParseError where
    show UnknownBotCmd = "unknown a bot command"
    show NoCmd = "required to fill out a bot command"

instance Exception ParseError

class BotCmdParser cmd client where
    getBotCmd :: MonadThrow m
        => cmd
        -> client 
        -> SourceCmd 
        -> ExpectedCmd
        -> m cmd
    getBotCmd tCmd tClient sourceCmd expectedCmd = 
        convert . unpack . toLower . pack $ sourceCmd where
        convert cmd
            | cmd == expectedCmd = pure tCmd 
            | otherwise = throwM UnknownBotCmd

    parsePayload :: MonadThrow m 
        => cmd
        -> client
        -> SourcePayload
        -> m Cmd

    parse :: MonadThrow m 
        => cmd
        -> client
        -> SourcePayload
        -> ExpectedCmd
        -> m (BotCmd cmd)
    parse tCmd tClient source expectedCmd = do
        parsedPayload <- parsePayload tCmd tClient source
        let sourceCmd = getCmd parsedPayload
        botCmd <- getBotCmd tCmd tClient sourceCmd expectedCmd
        let sourceArgs = getArgs parsedPayload
        let parsedCmd = BotCmd { cmd = botCmd, args = sourceArgs }

        pure parsedCmd

class (BotCmdParser cmd client) => BotInterpreter cmd client where
    run :: (MonadIO m, MonadThrow m)
        => cmd
        -> client
        -> SourcePayload
        -> ExpectedCmd
        -> m Response
    run tCmd tClient source expectedCmd = do
        botCmd <- parse tCmd tClient source expectedCmd
        result <- interpret botCmd tClient

        pure result

    interpret :: (MonadIO m, MonadThrow m)
        => BotCmd cmd
        -> client
        -> m Response