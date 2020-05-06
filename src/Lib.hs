{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( someFunc
    ) where

import Control.Monad.Except


someFunc :: (MonadError String m, Num b) => m b
someFunc = test "throw"
    where
        test "kek" = throwError "kek "
        test "throw" = throwError "throw error"
        test "ok" = pure 322

data Cmd = Test | Help | Echo | LastCmd deriving Show
instance Read Cmd where
    readsPrec _ input = toReadS parsed
        where 
            toReadS x = case x of (Just x) -> [(x, "")]; _ -> [];
            parsed :: Maybe Cmd
            parsed = case input of
                "/test" -> Just Test
                "/help" -> Just Help
                "/echo" -> Just Echo
                "/lst_cmd" -> Just LastCmd
                _ -> Nothing