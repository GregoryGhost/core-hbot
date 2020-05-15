{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger (
    LogLvl(..),
    doDebugLog,
    doNoticeLog,
    doWarnLog,
    doErrorLog,
    withLog
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
import Control.Exception (bracket)

data LogLvl = Debug | Notice | Warn | Error 
    deriving (Show, Eq, Generic)

type LogMsg = String

pureLog :: LogLvl 
    -> LogMsg 
    -> UTCTime 
    -> LogMsg
pureLog lvl msg time = logTime ++ s ++ logLvl ++ s ++ msg 
    where
        s = " : "
        logLvl = show lvl
        logTime = show time

doLog :: LogLvl -> Handle -> LogMsg -> IO ()
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
    buffering <- liftIO $ hSetBuffering fileHandler NoBuffering

    pure fileHandler

doDebugLog, doNoticeLog, doWarnLog, doErrorLog :: 
        Handle 
        -> LogMsg
        -> IO ()
doDebugLog = doLog Debug
doNoticeLog = doLog Notice
doWarnLog = doLog Warn
doErrorLog = doLog Error

withLog :: (Handle -> IO a) -> IO a
withLog f = bracket genLogFile hClose f
