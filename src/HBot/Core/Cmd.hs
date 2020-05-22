{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data BotCmd c a where
    BotCmd :: Cmd c a => { cmd :: c, args :: [a] } -> BotCmd c a

data ParseError = NoCmd | UnknownBotCmd
    deriving Typeable

instance Show ParseError where
    show UnknownBotCmd = "unknown a bot command"
    show NoCmd = "required to fill out a bot command"

instance Exception ParseError

class (Show a, Typeable a) => Interpretable a
class (Interpretable cmd, Interpretable args) => Cmd cmd args
class (Enum cmd, Bounded cmd, Show cmd) => Helpeable cmd where
    getHelp :: cmd -> String
    getHelp _ = Prelude.unlines $ Prelude.map show [(minBound :: cmd) ..]

class BotCmdParser cmd args client where
    parse :: MonadThrow m 
        => cmd
        -> args
        -> client
        -> SourcePayload
        -> m (BotCmd cmd args)

class (Show expected)
    => PureEval cmd args expected where
    
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