{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module HBot.Core.BasicCmd where

import HBot.Core.Cmd
import Data.Text as T
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Void
import Data.Typeable

data BasicCmd = Help | Kek deriving (Interpretable, Enum, Bounded, Helpeable)
data Messanger = Test

data EvalError = ItsKek deriving Typeable

instance Show EvalError where
    show ItsKek = "alert, it's kek =)"

instance Exception EvalError

instance Cmd BasicCmd String
instance Interpretable String

instance Show BasicCmd where
    show Help = "Help command about other commands"
    show Kek = "It's just kek and nothing"

instance PureEval BasicCmd String String where
    eval BotCmd { cmd = c } = pure . show $ c