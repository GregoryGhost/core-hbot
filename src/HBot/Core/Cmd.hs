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
type ExpectedType = String
type CmdArg = String
type CmdArgs = [CmdArg]

data BotCmd c a = BotCmd { cmd :: c, args :: [a] }

data ParseError = NoCmd | UnknownBotCmd
    deriving Typeable

instance Show ParseError where
    show UnknownBotCmd = "unknown a bot command"
    show NoCmd = "required to fill out a bot command"

instance Exception ParseError

class BotCmdParser cmd args client where
    parse :: MonadThrow m 
        => cmd
        -> args
        -> client
        -> SourcePayload
        -> m (BotCmd cmd args)

class (Show expected) => PureEval cmd args expected where
    eval :: (MonadThrow m) => BotCmd cmd args -> m expected

class (Show result, BotCmdParser cmd args client, PureEval cmd args result)
    => BotInterpreter cmd args result client where

    run :: (MonadIO m, MonadThrow m)
        => cmd
        -> args
        -> client
        -> SourcePayload
        -> m result
    run tCmd tArgs tClient source = do
        cmd <- parse tCmd tArgs tClient source

        interpret cmd tClient

    interpret :: (MonadIO m, MonadThrow m, Show result)
        => BotCmd cmd args
        -> client
        -> m result
    interpret cmd _ = eval cmd