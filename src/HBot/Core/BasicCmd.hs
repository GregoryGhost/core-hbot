{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HBot.Core.BasicCmd where

import HBot.Core.Cmd
import Data.Text as T
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Typeable

data BasicCmd = Help | Kek deriving Show
data Messanger = Test

data EvalError = ItsKek deriving Typeable

instance Show EvalError where
    show ItsKek = "alert, it's kek =)"

instance Exception EvalError

instance PureEval BasicCmd String BasicCmd where
    eval BotCmd { cmd = Help } = pure Kek
    eval BotCmd { cmd = Kek } = throwM ItsKek