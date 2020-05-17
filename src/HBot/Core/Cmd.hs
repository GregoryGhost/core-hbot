module HBot.Core.Cmd where

import Text.Read as T
import Control.Monad.IO.Class
import Control.Monad.Catch

data BasicCmd = Help deriving Show

type ParseError = String
type Response = String
type SourceCmd = String
type SourcePayload = String
data Cmd = Cmd
    {
        getCmd :: SourceCmd
        , getArgs :: CmdArgs
    }
type CmdArg = String
type CmdArgs = [CmdArg]

data BotCmd c = BotCmd { cmd :: c, args :: CmdArgs}

class BotCmdParser c where
    getBotCmd :: MonadThrow m => SourceCmd -> m c

    parsePayload :: MonadThrow m 
        => c
        -> SourcePayload
        -> m Cmd

    parse :: MonadThrow m => c -> SourcePayload -> m (BotCmd c)
    parse tCmd source = do
        parsedPayload <- parsePayload tCmd source
        botCmd <- getBotCmd $ getCmd parsedPayload
        let cmdArgs = getArgs parsedPayload
        let parsedCmd = BotCmd { cmd = botCmd, args = cmdArgs }

        pure parsedCmd

class (BotCmdParser cmd) => BotInterpreter cmd where
    run :: (MonadIO m, MonadThrow m)
        => cmd
        -> SourcePayload 
        -> m Response
    run cmd source = do
        botCmd <- parse cmd source
        result <- interpret botCmd

        pure result

    interpret :: (MonadIO m, MonadThrow m)
        => BotCmd cmd 
        -> m Response