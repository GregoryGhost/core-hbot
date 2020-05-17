{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module HBot.Core.BasicCmd where

import HBot.Core.Cmd
import Data.Text as T
import Control.Monad.Catch

data BasicCmd = Help deriving Show
data Messanger = Test

instance BotCmdParser BasicCmd Messanger where
    parsePayload :: MonadThrow m 
        => cmd
        -> client
        -> SourcePayload
        -> m Cmd
    parsePayload tCmd tClient source = do
        let splitted = Prelude.map T.unpack $ T.words $ T.pack source
        case Prelude.length splitted of 
            0 -> throwM NoCmd
            _ -> pure Cmd { getCmd = Prelude.head splitted,
            getArgs = Prelude.tail splitted }

-- instance BotCmdInterpreter Help Messanger where
--     interpret :: (MonadIO m, MonadThrow m)
--         => BotCmd cmd
--         -> client
--         -> m Response
--     --TODO:
