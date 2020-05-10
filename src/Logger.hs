{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger (
    SimpleLog(..),
    LogLvl(..),
    doDebugLog,
    doNoticeLog,
    doWarnLog,
    doErrorLog,
) where

import Control.Monad.Reader
import Control.Monad.IO.Class
import GHC.Generics
import Data.Time

data LogLvl = Debug | Notice | Warn | Error 
    deriving (Show, Eq, Generic)

type FileName = String
type LogMsg = String

type CommonLog m = (MonadIO m) => LogLvl -> FilePath -> LogMsg -> m ()
type SimpleLog m = MonadIO m => LogMsg -> m ()

pureLog :: LogLvl -> LogMsg -> UTCTime -> String
pureLog lvl msg time = logTime ++ s ++ logLvl ++ s ++ msg 
    where
        s = " : "
        logLvl = show lvl
        logTime = show time

doCommonLog :: CommonLog m
doCommonLog lvl path msg = write path pureLog
    where write msg = undefined

doLog :: MonadIO m => LogLvl -> LogMsg -> m ()
doLog lvl msg = do
    path <- genLogFileName
    m <- doCommonLog lvl path msg
    pure m

getTime :: (MonadIO m) => m Day
getTime = do
    t <- liftIO getCurrentTime
    let today = utctDay t
    pure today

genLogFileName :: (MonadIO m) => m String
genLogFileName = do
    time <- show <$> getTime
    let fileName = "log_" ++ time ++ ".log"
    pure fileName

doDebugLog :: SimpleLog m
doDebugLog = doLog Debug

doNoticeLog :: SimpleLog m
doNoticeLog = doLog Notice

doWarnLog :: SimpleLog m
doWarnLog = doLog Warn

doErrorLog :: SimpleLog m
doErrorLog = doLog Error