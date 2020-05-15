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