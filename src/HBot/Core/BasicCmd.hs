{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module HBot.Core.BasicCmd where

import HBot.Core.Cmd
import Data.Text as T
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Typeable
import Data.Void
import Data.Data
import Data.Maybe

-- data BasicCmd = Help | Kek deriving (Typeable, Interpretable)
-- data Messanger = Test

-- data EvalError = ItsKek deriving Typeable

-- instance Show EvalError where
--     show ItsKek = "alert, it's kek =)"

-- instance Exception EvalError

-- instance Cmd BasicCmd Void

-- instance Interpretable Void

-- getHelp :: Cmd cmd args -> String
-- getHelp tCmds = 

-- instance Show BasicCmd where
--     show Help = getHelp BasicCmd
--     show Kek = "It's just kek and nothing"

-- instance PureEval BasicCmd String String where
--     eval BotCmd { cmd = c } = pure . show $ c

--TODO: draft code

data Test = Kek1 | Kek2 | Kek3 deriving (Enum, Bounded, Typeable, Data, Cc)

instance Show Test where
    show Kek1 = "kek-cheburek1"
    show Kek2 = "kek-cheburek12"
    show Kek3 = "kek-cheburek3"

class (Enum a, Bounded a, Show a, Typeable a, Data a) => Cc a where
    getHelp :: a -> String 
    getHelp _ = Prelude.unlines $ Prelude.map show $ ac (minBound :: a)

    getHelp':: a -> String 
    getHelp' _ = Prelude.unlines $ Prelude.map show [(minBound :: a) ..]

    ac :: a -> [a]
    ac a = Prelude.map fromConstr $ dataTypeConstrs $ dataTypeOf (undefined :: a)