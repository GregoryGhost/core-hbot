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
    genLogFile,
    closeLog
) where

import Control.Monad.Reader
import Control.Monad.IO.Class
import GHC.Generics
import Data.Time
import System.IO
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Concurrent (newMVar, withMVar)

data LogLvl = Debug | Notice | Warn | Error 
    deriving (Show, Eq, Generic)

type FileName = String
type LogMsg = String

type SimpleLog m = MonadIO m => Handle -> LogMsg -> m ()

pureLog :: LogLvl -> LogMsg -> UTCTime -> String
pureLog lvl msg time = logTime ++ s ++ logLvl ++ s ++ msg 
    where
        s = " : "
        logLvl = show lvl
        logTime = show time

doLog :: MonadIO m => LogLvl -> Handle -> LogMsg -> m ()
doLog lvl fileHandle msg = do
    mutex <- liftIO $ newMVar ()
    time <- liftIO getCurrentTime

    let logMsg = pureLog lvl msg time
    log <- liftIO $ withMVar mutex (\() -> hPutStrLn fileHandle logMsg)

    pure log 

getTime :: (MonadIO m) => m Day
getTime = do
    t <- liftIO getCurrentTime
    let today = utctDay t

    pure today

genLogFile :: (MonadIO m) => m Handle
genLogFile = do
    time <- show <$> getTime
    let fileName = "log_" ++ time ++ ".log"
    currentDir <- liftIO getCurrentDirectory
    let filePath = currentDir </> fileName
    fileHandler <- liftIO $ openFile filePath WriteMode

    pure fileHandler

closeLog :: (MonadIO m) => Handle -> m ()
closeLog = liftIO . hClose

doDebugLog, doNoticeLog, doWarnLog, doErrorLog :: SimpleLog m
doDebugLog = doLog Debug
doNoticeLog = doLog Notice
doWarnLog = doLog Warn
doErrorLog = doLog Error