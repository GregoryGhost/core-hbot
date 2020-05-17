{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module HBot.Core.BasicCmd where

import HBot.Core.Cmd
import Data.Text as T
import Control.Monad.Catch
import Control.Monad.IO.Class

data BasicCmd = Help deriving Show
data Messanger = Test

instance BotCmdParser BasicCmd Messanger where
    parsePayload _ _ source = do
        let splitted = Prelude.map T.unpack $ T.words $ T.pack source
        case Prelude.length splitted of 
            0 -> throwM NoCmd
            _ -> pure Cmd { getCmd = Prelude.head splitted,
                            getArgs = Prelude.tail splitted }

instance BotInterpreter BasicCmd Messanger where
    interpret BotCmd { cmd = Help } _ = 
        pure "use this command to getting bot help"
