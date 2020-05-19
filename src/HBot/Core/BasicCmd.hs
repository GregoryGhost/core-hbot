{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module HBot.Core.BasicCmd where

import HBot.Core.Cmd
import Data.Text as T
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Typeable
import Data.Void

data BasicCmd = Help | Kek deriving (Typeable, Interpretable)
data Messanger = Test

data EvalError = ItsKek deriving Typeable

instance Show EvalError where
    show ItsKek = "alert, it's kek =)"

instance Exception EvalError

instance Cmd BasicCmd Void

instance Interpretable Void

getHelp tCmds = --TODO:

instance Show BasicCmd where
    show Help = getHelp BasicCmd
    show Kek = "It's just kek and nothing"

instance PureEval BasicCmd String String where
    eval BotCmd { cmd = c } = pure . show $ c