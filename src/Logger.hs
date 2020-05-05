module Logger (
    SimpleLog(..),
    doDebugLog,
    doNoticeLog,
    doWarnLog,
    doErrorLog
) where

import Control.Monad.Reader
import Control.Monad.IO.Class

data LogLvl = Debug | Notice | Warn | Error
    deriving (Show, Eq)

type FileName = String
type LogMsg = String
type CommonLog = (MonadIO m) => FilePath -> LogLvl -> LogMsg -> m ()
type SimpleLog = (MonadIO m) => LogMsg -> m ()

pureLog :: LogLvl -> LogMsg -> String
pureLog lvl msg = logLvl ++ s ++ logTime ++ s ++ logMsg 
    where
        s = ":"
        logLvl = print lvl
        logTime = "empty time: TODO"--TODO:

doLog :: CommonLog
doLog path lvl msg = undefined

doDebugLog :: SimpleLog
doWarnLog = doLog Debug

doNoticeLog :: SimpleLog
doWarnLog = doLog Notice

doWarnLog :: SimpleLog
doWarnLog = doLog Warn

doErrorLog :: SimpleLog
doWarnLog = doLog Error